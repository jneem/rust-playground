pub trait ApproxEq {
    fn approx_eq(self, other: Self) -> bool;
}

impl ApproxEq for f64 {
    fn approx_eq(self, other: f64) -> bool {
        let size = self.abs().max(other.abs());

        // We include the `self == other` check to allow for infinities.
        self == other || (self-other).abs() <= size.max(1.0) * 10e-5
    }
}

