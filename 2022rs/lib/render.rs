/** Render trait to draw stuff in a nice way */
use crate::array::Array2D;

pub trait Render {
    fn render(&self) -> String;
}

impl<T: Render> Render for Array2D<T> {
    fn render(&self) -> String {
        (0..self.nrow())
            .rev()
            .map(|row| {
                (0..self.ncol())
                    .map(|col| self[(row, col)].render())
                    .collect::<String>()
            })
            .collect::<Vec<_>>()
            .join("\n")
    }
}
