use std::rc::Rc;

use ketos::{Error, Value};
use ketos::name::{is_system_fn, is_system_operator, standard_names};
use ketos::compile::compile;
use ketos::rc_vec::{RcString, RcVec};

use crate::builtins;

pub fn exprs(interp: &builtins::Interpreter, exprs: &str, path: Option<String>) -> Result<Option<(Value, Value)>, Error> {
    let ketos_interp = interp.inner();
    let mut values = ketos_interp.parse_exprs(exprs, path)?;

    if values.len() == 0 {
        return Ok(None);
    }

    // Automatically insert parens if they're not explicitly put
    let input_value = match values.as_slice() {
        [Value::List(_)] => values.pop().unwrap(),
        _ => Value::List(RcVec::new(values))
    };

    let input_value = rewrite_exprs(interp, input_value);
    let code = compile(ketos_interp.context(), &input_value)?;
    let output_value = ketos_interp.execute_code(Rc::new(code))?;

    Ok(Some((input_value, output_value)))
}

fn rewrite_exprs(interp: &builtins::Interpreter, value: Value) -> Value {
    let ketos_interp = interp.inner();
    let scope = ketos_interp.scope();

    match value {
        Value::List(list) => {
            let list_v = list.into_vec();
            let mut iter = list_v.into_iter();
            let first_value = iter.next().unwrap();
            let mut new_list = vec![];

            if let Value::Name(first_name) = first_value {
                if is_system_operator(first_name) {
                    // These system operators need special handling because they
                    // support lists where the first value can be a bare name

                    new_list.push(Value::Name(first_name));

                    match first_name {
                        standard_names::LET | standard_names::DEFINE | standard_names::MACRO | standard_names::LAMBDA => {
                            if let Some(first_arg) = iter.next() {
                                new_list.push(first_arg);
                            }
                        }
                        standard_names::STRUCT | standard_names::EXPORT | standard_names::USE => {
                            while let Some(arg) = iter.next() {
                                new_list.push(arg);
                            }
                        }
                        _ => ()
                    }
                } else if !is_system_fn(first_name) && !scope.contains_name(first_name) {
                    // Looks like this expr is shaped like a function call, to a
                    // function that does not exist. Change this into a call to
                    // `proc`.

                    new_list.push(Value::Name(interp.proc_name));
                    new_list.push({
                        let name_store = scope.borrow_names();
                        let first_name_str = name_store.get(first_name).to_string();
                        Value::String(RcString::new(first_name_str.clone()))
                    });
                } else {
                    new_list.push(first_value);
                }
            } else {
                new_list.push(first_value);
            }

            new_list.extend(iter.map(|value| rewrite_exprs(interp, value)));
            Value::List(RcVec::new(new_list))
        }
        Value::Name(name) if !is_system_fn(name) && !is_system_operator(name) && !scope.contains_name(name) => {
            let name_store = scope.borrow_names();
            let arg_str = name_store.get(name).to_string();
            Value::String(RcString::new(arg_str))
        }
        _ => value
    }
}
