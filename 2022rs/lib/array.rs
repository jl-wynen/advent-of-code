use std::cmp::Ordering;
/** Multi dimensional arrays */
use std::ops::{Index, IndexMut};

#[derive(Clone)]
pub struct Array2D<T> {
    nrow_: usize,
    ncol_: usize,
    elems_: Vec<T>,
}

impl<T: Clone> Array2D<T> {
    pub fn new(nrow: usize, ncol: usize, init: T) -> Self {
        Self {
            nrow_: nrow,
            ncol_: ncol,
            elems_: vec![init; nrow * ncol],
        }
    }

    pub fn roll_rows(&mut self, by: isize, init: T) {
        let mut new_elems = vec![init; self.nrow_ * self.ncol_];

        match by.cmp(&0) {
            Ordering::Greater => {
                let by = by as usize;
                for row in 0..(self.nrow_ - by) {
                    for col in 0..self.ncol_ {
                        new_elems[(row + by) * self.ncol_ + col] = self[(row, col)].clone();
                    }
                }
            }
            Ordering::Less => {
                let by = (-by) as usize;
                for row in by..self.nrow_ {
                    for col in 0..self.ncol_ {
                        new_elems[(row - by) * self.ncol_ + col] = self[(row, col)].clone();
                    }
                }
            }
            Ordering::Equal => {}
        }
        self.elems_ = new_elems
    }
}

impl<T> Array2D<T> {
    pub fn nrow(&self) -> usize {
        self.nrow_
    }
    pub fn ncol(&self) -> usize {
        self.ncol_
    }
}

impl<T> Index<(usize, usize)> for Array2D<T> {
    type Output = T;

    fn index(&self, (row, col): (usize, usize)) -> &Self::Output {
        &self.elems_[row * self.ncol_ + col]
    }
}

impl<T> IndexMut<(usize, usize)> for Array2D<T> {
    fn index_mut(&mut self, (row, col): (usize, usize)) -> &mut Self::Output {
        &mut self.elems_[row * self.ncol_ + col]
    }
}
