use std::fmt::Debug;
use std::collections::BTreeSet;


#[derive(Clone, Debug)]
pub struct Scope<T>
    where T : Debug + Clone
{
    pub index: usize,
    pub path: Vec<usize>,

    // Keep name + values in vec in order to preserve order.
    // Using HashMap had the issue of essentially randomizing
    // the parameter order, which is less than ideal in any case.
    pub names: Vec<String>,
    pub values: Vec<T>,
}

#[derive(Clone, Debug)]
pub struct Module<T>
    where T : Debug + Clone,
{
    pub scopes: Vec<Scope<T>>,
    pub current_scope_index: usize,
}

impl <T> Module<T>
    where T : Debug + Clone
{
    pub fn new() -> Self
    {
        Self {
            scopes: Vec::with_capacity(128),
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
            _ => Vec::with_capacity(128)
        };

        let mut new_scope_names  = Vec::with_capacity(128);
        let mut new_scope_values = Vec::with_capacity(128);

        if let Some(parent_scope) = parent_scope {
            let parent_scope       = &self.scopes[parent_scope.index];

            // TODO: clone + append? chained? extend_from_slice? Something else?
            new_scope_names.extend_from_slice(&parent_scope.names);
            new_scope_values.extend_from_slice(&parent_scope.values);
        }

        let new_scope = Scope {
            index: self.scopes.len(),
            path: new_scope_path,
            names: new_scope_names,
            values: new_scope_values,
        };

        self.current_scope_index = new_scope.index;
        self.scopes.push(new_scope);
    }

    pub fn end_scope(&mut self)
    {
        let scope = &self.scopes[self.current_scope_index];
        if scope.path.is_empty() { return }

        self.current_scope_index = *scope.path.last().unwrap();
    }

    pub fn get(&self, name: &str) -> Option<&T>
    {
        let scope = self.current_scope();
        let index = scope.names.iter().rposition(|n| n == name)?;

        Some(&scope.values[index])
    }

    pub fn index_of(&self, scope: usize, name: &str) -> Option<usize>
    {
        let scope = self.scopes.get(scope)?;
        scope.names.iter().rposition(|n| n == name)
    }

    pub fn get_at(&self, scope: usize, index: usize) -> &T
    {
        let scope = self.scopes.get(scope).unwrap();
        &scope.values[index]
    }

    pub fn get_from_scope(&self, scope: usize, name: &str) -> Option<&T>
    {
        let scope = self.scopes.get(scope)?;
        let index = scope.names.iter().rposition(|n| n == name)?;

        Some(&scope.values[index])
    }

    pub fn add_to_current(&mut self, name: &str, value: T) -> usize
    {
        let scope = self.current_scope_mut();
        scope.names.push(name.to_owned());
        scope.values.push(value);
        scope.values.len() - 1
    }

    pub fn update_in_current(&mut self, index: usize, value: T)
    {
        let scope = self.current_scope_mut();
        scope.values[index] = value;
    }

    pub fn captures(&self) -> Vec<String>
    {
        let scope = self.current_scope();

        let global_scope = &self.scopes[0];
        let globals      = global_scope.names.iter().map(std::string::String::as_str).collect::<Vec<&str>>();
        let to_remove    = BTreeSet::<&str>::from_iter(globals);

        let capacity              = scope.names.len() - global_scope.names.len();
        let mut vars: Vec<String> = Vec::with_capacity(capacity);

        vars.append(&mut scope.names.clone());
        vars.retain(|n| !to_remove.contains(n.as_str()));
        
        vars
    }
}

impl<T> Default for Module<T>
    where T : Debug + Clone
{
    fn default() -> Self
    {
        Self::new()
    }
}
