use std::cmp::Ordering;

use super::{Error, Result};
use crate::common::function::UpValue;

/// A list of open UpValues.
/// The values are in the descending order of UpValue::get_index.
/// (i.e., the last element of the list is close to the bottom of the stack.)
pub(super) struct UpValueList {
    head: Option<Box<UpValueNode>>,
}

struct UpValueNode {
    upvalue: UpValue,
    next: Option<Box<UpValueNode>>,
}

impl UpValueList {
    pub fn new() -> Self {
        Self { head: None }
    }

    /// Inserts a new node containing `upvalue`.
    /// Does nothing if there is an exiting node with its index set to
    /// `upvalue.get_index()` in the list.
    /// Returns an Err(e) when any upvalue touched in this function is a closed one.
    pub fn insert(&mut self, upvalue: UpValue) -> Result<()> {
        let stack_index = upvalue
            .get_index()
            .map_err(Error::InvalidUpValueOperationError)?;
        let mut cur = &mut self.head;
        loop {
            match cur {
                None => {
                    std::mem::swap(
                        cur,
                        &mut Some(Box::new(UpValueNode {
                            upvalue,
                            next: None,
                        })),
                    );
                    break;
                }
                Some(_) => {
                    let index = cur
                        .as_ref()
                        .unwrap()
                        .upvalue
                        .get_index()
                        .map_err(Error::InvalidUpValueOperationError)?;
                    match index.cmp(&stack_index) {
                        Ordering::Equal => break, // the value is already tracked.
                        Ordering::Greater => cur = &mut cur.as_mut().unwrap().next,
                        Ordering::Less => {
                            let new_node = Box::new(UpValueNode {
                                upvalue,
                                next: cur.take(),
                            });
                            std::mem::swap(cur, &mut Some(new_node));
                            break;
                        }
                    }
                }
            }
        }
        Ok(())
    }

    /// Closes the open upvalue with its index equal to or greater than `index` in the list.
    pub fn close_upvalues_at_and_above(&mut self, index: usize) -> Result<()> {
        let cur = &mut self.head;
        loop {
            match cur {
                Some(_) => {
                    let cur_index = cur
                        .as_ref()
                        .unwrap()
                        .upvalue
                        .get_index()
                        .map_err(Error::InvalidUpValueOperationError)?;
                    if cur_index < index {
                        break;
                    }
                    let mut found = cur.take().unwrap();
                    std::mem::swap(cur, &mut found.next);
                    found
                        .upvalue
                        .close()
                        .map_err(Error::InvalidUpValueOperationError)?;
                }
                None => break,
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::{cell::RefCell, rc::Rc};

    use crate::common::{value::Value, Stack};

    use super::*;

    fn get_index_vector(list: &UpValueList) -> Result<Vec<usize>> {
        let mut indices = Vec::new();
        let mut node = list.head.as_ref();
        while let Some(v) = node {
            indices.push(
                v.upvalue
                    .get_index()
                    .map_err(Error::InvalidUpValueOperationError)?,
            );
            node = v.next.as_ref();
        }
        Ok(indices)
    }

    fn get_upvalue_state_vector(list: &UpValueList) -> Vec<bool> {
        let mut states = Vec::new();
        let mut node = list.head.as_ref();
        while let Some(v) = node {
            states.push(v.upvalue.is_closed());
            node = v.next.as_ref();
        }
        states
    }

    #[test]
    fn insertion_sorts_nodes() -> Result<()> {
        let mut list = UpValueList::new();
        let stack = Rc::new(RefCell::new(Stack::new()));
        list.insert(UpValue::new(2, stack.clone()))?;
        list.insert(UpValue::new(0, stack.clone()))?;
        list.insert(UpValue::new(1, stack.clone()))?;
        assert_eq!(get_index_vector(&list)?, vec![2, 1, 0]);
        Ok(())
    }

    #[test]
    fn close_upvalues_at_and_above_does_what_it_says() -> Result<()> {
        let mut list = UpValueList::new();
        let stack = Rc::new(RefCell::new(Stack::new()));
        stack.borrow_mut().push(Value::Number(0.0));
        stack.borrow_mut().push(Value::Number(1.0));
        stack.borrow_mut().push(Value::Number(2.0));
        let two = UpValue::new(2, stack.clone());
        let one = UpValue::new(1, stack.clone());
        list.insert(two.clone())?;
        list.insert(UpValue::new(0, stack.clone()))?;
        list.insert(one.clone())?;
        list.close_upvalues_at_and_above(1)?;
        assert!(two.is_closed());
        assert!(one.is_closed());
        assert_eq!(get_upvalue_state_vector(&list), vec![false]);
        Ok(())
    }
}
