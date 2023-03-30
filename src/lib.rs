use std::sync::Arc;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};

use pyo3::prelude::*;
use pyo3::exceptions::PyValueError;
use pyo3::exceptions::PyTypeError;
use pyo3::types::PyType;
use pyo3::class::basic::CompareOp;


#[derive(PartialEq, Eq, Hash, Clone)]
enum AtomValue {
    Str(String),
    Int(i64),
}

#[derive(PartialEq, Eq, Hash)]
struct AtomType {
    name: String,
    typ: String,
}

#[derive(PartialEq, Eq)]
struct Atom {
    atom_type: Arc<AtomType>,
    value: AtomValue,
}

// We don't implement FromPyObject because we need to know whether we are
// expecting str or int.
impl AtomValue {
    fn from_pyobject(typ: &str, value: PyObject, py: Python <'_>) -> PyResult<AtomValue> {
        match typ {
            "int" => Ok(AtomValue::Int(value.extract(py)?)),
            "str" => Ok(AtomValue::Str(value.extract(py)?)),
            _ => Err(PyTypeError::new_err("bad internal type."))
        }
    }
}

impl IntoPy<PyObject> for AtomValue {
    fn into_py(self, py: Python <'_>) -> PyObject {
        match self {
            Self::Str(value) => value.into_py(py),
            Self::Int(value) => value.into_py(py),
        }
    }
}

#[pyclass(name="AtomType")]
struct PyAtomType {
    atom_type: Arc<AtomType>,
}

#[pyclass(name="Atom")]
struct PyAtom {
    atom: Arc<Atom>,
}

#[pymethods]
impl PyAtom {
    fn __repr__(&self) -> String {
        let name = &self.atom.atom_type.name;
        match &self.atom.value {
            AtomValue::Str(val) => format!("{}('{}')", name, val),
            AtomValue::Int(val) => format!("{}({})", name, val),
        }
    }

    fn __str__(&self) -> String {
        match &self.atom.value {
            AtomValue::Str(val) => format!("{}", val),
            AtomValue::Int(val) => format!("{}", val),
        }
    }

    #[getter]
    fn atom_type(&self) -> PyAtomType {
        PyAtomType { atom_type: self.atom.atom_type.clone() }
    }

    #[getter]
    fn value(&self, py: Python<'_>) -> PyObject {
        self.atom.value.clone().into_py(py)
    }

    fn __richcmp__(&self, other: &Self, op: CompareOp, py: Python<'_>) -> PyObject {
        match op {
            CompareOp::Eq => (self.atom == other.atom).into_py(py),
            CompareOp::Ne => (self.atom != other.atom).into_py(py),
            _ => py.NotImplemented(),
        }
    }

    fn __hash__(&self) -> u64 {
        let mut hasher = DefaultHasher::new();
        self.atom.value.hash(&mut hasher);
        hasher.finish()
    }
}

#[pymethods]
impl PyAtomType {
    #[new]
    fn py_new(name: String, typ: &PyType) -> PyResult<Self> {

        let typ = typ.name()?;
        let atom_type = Arc::new(AtomType{ name, typ: typ.to_string() });

        match typ {
            "int" | "str" => Ok(PyAtomType{ atom_type }),
            _ => Err(PyValueError::new_err("typ must be int or str.")),
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

    fn __call__(&self, value: PyObject, py: Python<'_>) -> PyResult<PyAtom> {
        let atom_type = self.atom_type.clone();
        let value = AtomValue::from_pyobject(&atom_type.typ, value, py)?;
        let atom = Arc::new(Atom{ atom_type, value });
        Ok(PyAtom{ atom })
    }
}

/// A Python module implemented in Rust.
#[pymodule]
fn rust_protosym(_py: Python, m: &PyModule) -> PyResult<()> {
    m.add_class::<PyAtom>()?;
    m.add_class::<PyAtomType>()?;
    Ok(())
}
