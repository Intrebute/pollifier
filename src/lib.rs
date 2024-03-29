use std::borrow::Borrow;
use std::collections::HashMap;
use std::hash::Hash;

/// Encodes possible transitions between two states.
#[derive(Ord, PartialOrd, Eq, PartialEq, Copy, Clone, Debug)]
pub enum EdgeState {
    Low,
    Rise,
    Fall,
    High,
}


impl From<SignalState> for EdgeState {
    fn from(s: SignalState) -> Self {
        match s {
            SignalState::High => EdgeState::High,
            SignalState::Low => EdgeState::Low,
        }
    }
}

impl EdgeState {
    /// Computes the transition between two signal states
    pub fn new_transition(prev: SignalState, next: SignalState) -> Self {
        use EdgeState as ES;
        use SignalState as SS;
        match (prev, next) {
            (SS::Low, SS::Low) => ES::Low,
            (SS::Low, SS::High) => ES::Rise,
            (SS::High, SS::Low) => ES::Fall,
            (SS::High, SS::High) => ES::High,
        }
    }

    /// Computes the final signal as a result of a transition. Essentially forgets the past signal state.
    pub fn current_signal(self) -> SignalState {
        use EdgeState as ES;
        use SignalState as SS;
        match self {
            ES::High | ES::Rise => SS::High,
            ES::Low | ES::Fall => SS::Low,
        }
    }
}

/// Encodes a two-valued signal.
#[derive(PartialEq, Eq, Copy, Clone, Debug)]
pub enum SignalState {
    High,
    Low,
}

/// Represents a boolean value that might change many times, but only the last change before an
/// an explicit `clock` action is made available.
#[derive(PartialEq, Eq, Copy, Clone, Debug)]
pub enum Clocker {
    /// A value that has not been updated since the last `clock`.
    Idle(SignalState),
    /// A value that has been updated at least once since the last `clock`, not necessarily
    /// distinct from the previous value.
    Touched {
        previous: SignalState,
        next: SignalState,
    },
}

impl Clocker {
    pub fn new_idle(s: SignalState) -> Self {
        Clocker::Idle(s)
    }

    pub fn new_touched(prev: SignalState, next: SignalState) -> Self {
        Clocker::Touched {
            previous: prev,
            next,
        }
    }

    /// Stabilizes the current value. Meant to be used at the end of a "clock cycle".
    /// 
    /// returns the transition between the previous and the new current state.
    pub fn clock(&mut self) -> EdgeState {
        match *self {
            Clocker::Touched { previous, next } => {
                *self = Clocker::Idle(next);
                return EdgeState::new_transition(previous, next);
            }

            Clocker::Idle(s) => {
                return EdgeState::from(s);
            }
        }
    }

    /// Changes the value of the signal. 
    /// 
    /// This new state will not be reflected until `clock` is called.
    pub fn touch(&mut self, new_signal: SignalState) {
        match *self {
            Clocker::Touched { ref mut next, .. } => {
                *next = new_signal;
            }
            Clocker::Idle(s) => {
                *self = Clocker::Touched {
                    previous: s,
                    next: new_signal,
                };
            }
        }
    }

    /// Returns the last `clock`ed value.
    pub fn get(&self) -> SignalState {
        match *self {
            Clocker::Idle(ss) => ss,
            Clocker::Touched { previous, .. } => previous,
        }
    }
}

/// A map from values `T` to dynamic `clock`able signals.
/// 
/// The signal value for a key that has not been `touch`ed or 
/// `clock`ed yet is implicitly considered a `Low`.
pub struct Pollifier<T>(HashMap<T, (Clocker, EdgeState)>);

impl<T> Pollifier<T> {
    pub fn new() -> Self {
        Pollifier(HashMap::default())
    }
}

impl<T> Pollifier<T>
where
    T: Hash + Eq,
{
    /// Returns the transition between last and current `clock`ed value associated with `k`.
    /// 
    /// If the signal for `k` has not been `touch`ed or `clock`ed, it is considered `Low`.
    /// 
    /// To get the current value, instead of the transition between values, call [`current_signal()`] on the returned value.
    pub fn get<Q>(&self, k: &Q) -> EdgeState
    where
        T: Borrow<Q>,
        Q: Hash + Eq,
    {
        self.0.get(k).map(|(_, e)| *e).unwrap_or(EdgeState::Low)
    }
}

impl<T> Pollifier<T>
where
    T: Hash + Eq,
{
    /// Sets the state for the signal associated with `k`.
    /// 
    /// This new state will not be reflected until `clock` is called.
    pub fn touch(&mut self, k: T, ns: SignalState) {
        self.0
            .entry(k)
            .and_modify(|(c, _)| {
                c.touch(ns);
            })
            .or_insert((Clocker::new_touched(SignalState::Low, ns), EdgeState::Low));
    }

    /// Stabilizes the current state for every `touch`ed key.
    pub fn clock(&mut self) {
        for (ref mut cl, ref mut es) in self.0.values_mut() {
            *es = cl.clock();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::EdgeState as ES;
    use super::SignalState as SS;
    use super::*;
    #[test]
    fn signal_to_constant_edge() {
        assert_eq!(ES::from(SS::High), ES::High);
        assert_eq!(ES::from(SS::Low), ES::Low);
    }

    #[test]
    fn signal_to_edge_transition() {
        assert_eq!(ES::new_transition(SS::Low, SS::Low), ES::Low);
        assert_eq!(ES::new_transition(SS::Low, SS::High), ES::Rise);
        assert_eq!(ES::new_transition(SS::High, SS::High), ES::High);
        assert_eq!(ES::new_transition(SS::High, SS::Low), ES::Fall);
    }

    #[test]
    fn clocker_same_until_clocked() {
        let mut c: Clocker = Clocker::new_touched(SS::Low, SS::High);
        assert_eq!(c.get(), SS::Low);

        c.touch(SS::Low);
        assert_eq!(c.get(), SS::Low);

        c.touch(SS::High);
        assert_eq!(c.get(), SS::Low);

        c.clock();
        assert_eq!(c.get(), SS::High);

        let mut c: Clocker = Clocker::new_idle(SS::High);
        assert_eq!(c.get(), SS::High);

        c.touch(SS::High);
        assert_eq!(c.get(), SS::High);

        c.touch(SS::Low);
        assert_eq!(c.get(), SS::High);

        c.clock();
        assert_eq!(c.get(), SS::Low);

        let mut c: Clocker = Clocker::new_touched(SS::Low, SS::High);
        c.touch(SS::Low);
        assert_eq!(c.clock(), ES::Low);
        c.touch(SS::High);
        assert_eq!(c.get(), SS::Low);
        assert_eq!(c.clock(), ES::Rise);
    }

    #[test]
    fn pollifier_works() {
        let mut p: Pollifier<bool> = Pollifier::new();
        assert_eq!(p.get(&true), EdgeState::Low);
        assert_eq!(p.get(&false), EdgeState::Low);

        p.touch(true, SignalState::High);

        assert_eq!(p.get(&true), EdgeState::Low);
        assert_eq!(p.get(&false), EdgeState::Low);

        p.clock();

        assert_eq!(p.get(&true), EdgeState::Rise);
        assert_eq!(p.get(&false), EdgeState::Low);

        p.clock();

        assert_eq!(p.get(&true), EdgeState::High);
        assert_eq!(p.get(&false), EdgeState::Low);

        p.touch(true, SignalState::Low);

        assert_eq!(p.get(&true), EdgeState::High);
        assert_eq!(p.get(&false), EdgeState::Low);

        p.clock();

        assert_eq!(p.get(&true), EdgeState::Fall);
        assert_eq!(p.get(&false), EdgeState::Low);

        p.clock();

        assert_eq!(p.get(&true), EdgeState::Low);
        assert_eq!(p.get(&false), EdgeState::Low);
    }
}
