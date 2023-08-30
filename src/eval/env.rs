struct Env<'a> {
    store: BTreeMap<&'a str, Value>,
    parent: Rc<Env<'a>>,
}

impl<'a> Env<'a> {

}

#[cfg(test)]
mod tests {
    
}