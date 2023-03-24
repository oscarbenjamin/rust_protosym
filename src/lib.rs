use std::sync::Arc;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};

use pyo3::prelude::*;
use pyo3::exceptions::PyValueError;
use pyo3::exceptions::PyTypeError;
use pyo3::types::PyType;
use pyo3::class::basic::CompareOp;


#[derive(PartialEq, Eq, Hash)]
enum AtomValue {
    Str(String),
    Int(i64),
}

#[derive(PartialEq, Eq, Hash)]
struct AtomType {
    name: String,
    typ: String,
}

#[pyclass(name="AtomType")]
struct PyAtomType {
    atom_type: Arc<AtomType>,
}

#[pyclass]
struct Atom {
    atom_type: Arc<AtomType>,
    value: AtomValue,
}

#[pymethods]
impl Atom {
    fn __repr__(&self) -> String {
        let name = &self.atom_type.name;
        match &self.value {
            AtomValue::Str(string) => format!("{}('{}')", name, string),
            AtomValue::Int(integer) => format!("{}({})", name, integer),
        }
    }

    fn __str__(&self) -> String {
        match &self.value {
            AtomValue::Str(string) => format!("{}", string),
            AtomValue::Int(integer) => format!("{}", integer),
        }
    }

    #[getter]
    fn atom_type(&self) -> PyAtomType {
        PyAtomType { atom_type: self.atom_type.clone() }
    }

    #[getter]
    fn value(&self, py: Python<'_>) -> PyObject {
        match &self.value {
            AtomValue::Str(string) => string.into_py(py),
            AtomValue::Int(integer) => integer.into_py(py),
        }
    }

    fn __richcmp__(&self, other: &Self, op: CompareOp, py: Python<'_>) -> PyObject {
        match op {
            CompareOp::Eq => (self.atom_type == other.atom_type && self.value == other.value).into_py(py),
            CompareOp::Ne => (self.atom_type != other.atom_type || self.value != other.value).into_py(py),
            _ => py.NotImplemented(),
        }
    }

    fn __hash__(&self) -> u64 {
        let mut hasher = DefaultHasher::new();
        self.value.hash(&mut hasher);
        hasher.finish()
    }
}

#[pymethods]
impl PyAtomType {
		#[new]
    fn py_new(name: String, typ: &PyType) -> PyResult<Self> {

        if let Ok(type_name) = typ.name() {
            if type_name == "int" {
                Ok(PyAtomType { atom_type: Arc::new(
                  AtomType { name: name , typ: "int".to_string() }) } )
            } else if type_name == "str" {
                Ok(PyAtomType { atom_type: Arc::new(
                  AtomType { name: name , typ: "str".to_string() }) } )
            } else {
                Err(PyValueError::new_err("typ must be int or str."))
            }
        } else {
            Err(PyTypeError::new_err("typ must be a type."))
        }
    }

    fn __richcmp__(&self, other: &Self, op: CompareOp, py: Python<'_>) -> PyObject {
        match op {
            CompareOp::Eq => (self.atom_type == other.atom_type).into_py(py),
            CompareOp::Ne => (self.atom_type != other.atom_type).into_py(py),
            _ => py.NotImplemented(),
        }
    }

    fn __hash__(&self) -> u64 {
        let mut hasher = DefaultHasher::new();
        self.atom_type.hash(&mut hasher);
        hasher.finish()
    }

    fn __repr__(&self) -> String {
        self.atom_type.name.clone()
    }

    fn from_int(&self, value: i64) -> PyResult<Atom> {
        if self.atom_type.typ == "int" {
            Ok(Atom { atom_type: self.atom_type.clone() , value: AtomValue::Int(value) } )
        } else {
            Err(PyTypeError::new_err("value must be of type int."))
        }
    }

    fn from_str(&self, value: String) -> PyResult<Atom> {
        if self.atom_type.typ == "str" {
            Ok(Atom { atom_type: self.atom_type.clone() , value: AtomValue::Str(value) } )
        } else {
            Err(PyTypeError::new_err("value must be of type int."))
        }
    }

    fn __call__(&self, value: PyObject) -> PyResult<Atom> {
        if self.atom_type.typ == "int" {
            Python::with_gil(|py| -> PyResult<Atom> {
                let value_i64 = value.extract(py)?;
                self.from_int(value_i64)
            })
        } else if self.atom_type.typ == "str" {
            Python::with_gil(|py| -> PyResult<Atom> {
                let value_str = value.extract(py)?;
                self.from_str(value_str)
            })
        } else {
            Err(PyTypeError::new_err("bad internal type."))
        }
    }
}

/// A Python module implemented in Rust.
#[pymodule]
fn rust_protosym(_py: Python, m: &PyModule) -> PyResult<()> {
    m.add_class::<Atom>()?;
    m.add_class::<PyAtomType>()?;
    Ok(())
}
