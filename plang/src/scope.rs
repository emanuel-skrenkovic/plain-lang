use std::collections::HashMap;
use std::fmt::Debug;


#[derive(Debug)]
pub struct Scope<T>
    where T : Debug
{
    pub index: usize,
    pub path: Vec<usize>,
    pub values: HashMap<String, T>,
}

#[derive(Debug)]
pub struct Module<T>
    where T : Debug
{
    pub scopes: Vec<Scope<T>>,
    pub current_scope_index: usize,
}

impl <T> Module<T>
    where T : Debug
{
    pub fn new() -> Self
    {
        Self {
            scopes: Vec::with_capacity(1024),
            current_scope_index: 0,
        }
    }

    pub fn current_scope(&self) -> &Scope<T>
    {
        &self.scopes[self.current_scope_index]
    }

    pub fn current_scope_mut(&mut self) -> &mut Scope<T>
    {
        &mut self.scopes[self.current_scope_index]
    }

    pub fn begin_scope(&mut self)
    {
        let parent_scope = if self.scopes.is_empty() { None }
                           else                      { Some(&self.scopes[self.current_scope_index]) };

        // New scope path will contain the parent as well, so extending with the
        // index of the parent.
        let new_scope_path = match parent_scope {
            Some(parent_scope) => {
                let mut new_scope_path = Vec::with_capacity(parent_scope.path.len() + 1);
                new_scope_path.extend_from_slice(&parent_scope.path);
                new_scope_path.push(parent_scope.index);
                new_scope_path
            }
            _ => vec![],
        };

        let new_scope = Scope {
            index: self.scopes.len(),
            path: new_scope_path,
            values: HashMap::new(),
        };

        self.current_scope_index = new_scope.index;
        self.scopes.push(new_scope);
    }

    pub fn end_scope(&mut self)
    {
        let scope = &self.scopes[self.current_scope_index];
        if scope.path.is_empty() { return }

        let parent_scope         = scope.path.last().unwrap();
        self.current_scope_index = *parent_scope;
    }

    pub fn get(&self, name: &str) -> Option<&T>
    {
        let scope = self.current_scope();

        let val = scope.values.get(name);
        if val.is_some() {
            return val
        }

        for i in scope.path.iter().rev() {
            let scope = &self.scopes[*i];

            let val = scope.values.get(name);
            if val.is_some() {
                return val
            }
        }

        None
    }

    pub fn captures(&self) -> Vec<String>
    {
        let scope = self.current_scope();

        let mut vars = Vec::with_capacity(1024);

        for key in scope.values.keys() {
            vars.push(key.clone());
        }

        for i in scope.path.iter().filter(|s| **s != 0) {
            let closed_scope = &self.scopes[*i];

            for key in closed_scope.values.keys() {
                vars.push(key.clone());
            }
        }

        vars
    }
}

impl<T> Default for Module<T>
    where T : Debug
{
    fn default() -> Self
    {
        Self::new()
    }
}
