//
// Source for rust_protosym.rust_protosym module.
//
// Defines rust types and also Python wrappers for those types. The intention
// is that the types exposed to Python are interchangeable with the Python
// classes defined in protosym.
//
// Classes exposed from here:
//    AtomType
//    Atom
//    TreeExpr
//    TreeNode
//    TreeAtom
//
// The Rust code here does not need the latter two classes and instead just has:
//    AtomType
//    Atom
//    Tree
//
// Maybe the Python code should be changed to the same.
//
// For now the Rust code can only handle Atoms with an internal value that is a
// string or an integer and also for integers it only uses 64 bit signed
// integers. The intention is certainly to improve that.
//

use std::fmt;
use std::iter::once;
use std::sync::{Arc, Mutex};
use std::collections::hash_map::DefaultHasher;
use std::collections::{HashMap, HashSet};
use std::hash::{Hash, Hasher};

use pyo3::prelude::*;
use pyo3::exceptions::PyValueError;
use pyo3::exceptions::PyTypeError;
use pyo3::types::PyType;
use pyo3::types::PyTuple;
use pyo3::class::basic::CompareOp;
use pyo3::once_cell::GILOnceCell;

use internment::ArcIntern;


// ---- The type of values held in an Atom is AtomValue


#[derive(PartialEq, Eq, Hash, Clone)]
enum AtomValue {
    Str(String),
    Int(i64),
}


// ---- Internal rust structs


#[derive(PartialEq, Eq, Hash, Clone)]
struct _AtomType {
    name: String,
    type_name: String,
}


#[derive(PartialEq, Eq, Hash, Clone)]
struct _Atom {
    atom_type: AtomType,
    value: AtomValue,
}


#[derive(PartialEq, Eq, Hash, Clone)]
enum TreeNode {
    Atom(Atom),
    Node(Vec<Tree>),
}


// ------------------- The public rust level structs


#[derive(PartialEq, Eq, Hash, Clone)]
struct AtomType {
    _atom_type: Arc<_AtomType>,
}


#[derive(PartialEq, Eq, Hash, Clone)]
struct Atom {
    _atom: _Atom,
}


#[derive(PartialEq, Eq, Hash, Clone)]
struct Tree {
    node: ArcIntern<TreeNode>,
}


// ------------------- Structs for the Python objects


#[pyclass(name="AtomType")]
#[derive(PartialEq, Eq, Hash, Clone)]
struct PyAtomType {
    atom_type: AtomType,
}


#[pyclass(name="Atom")]
#[derive(PartialEq, Eq, Hash, Clone)]
struct PyAtom {
    atom: Atom,
}


#[pyclass(name="TreeExpr", subclass)]
#[derive(PartialEq, Eq, Hash, Clone)]
struct PyTreeExpr {
    tree: Tree,
}


// The way things work out here it would be better not to have PyTreeAtom and
// PyTreeNode as distinct from PyTreeExpr.


#[pyclass(extends=PyTreeExpr, name="TreeAtom")]
#[derive(PartialEq, Eq, Hash, Clone)]
struct PyTreeAtom {}


#[pyclass(extends=PyTreeExpr, name="TreeNode")]
#[derive(PartialEq, Eq, Hash, Clone)]
struct PyTreeNode {}


// ---------------------------------------- Construct Interned Tree


impl AtomType {

    fn new(name: &str, type_name: &str) -> AtomType {
        let name = name.to_string();
        let type_name = type_name.to_string();
        let _atom_type = Arc::new(_AtomType{ name, type_name });
        AtomType { _atom_type }
    }

    fn name(&self) -> &String {
        &self._atom_type.name
    }

    fn type_name(&self) -> &String {
        &self._atom_type.type_name
    }

}


impl Atom {

    fn new(atom_type: AtomType, value: AtomValue) -> Atom {
        let _atom = _Atom { atom_type, value };
        Atom { _atom }
    }

    fn atom_type(&self) -> &AtomType {
        &self._atom.atom_type
    }

    fn value(&self) -> &AtomValue {
        &self._atom.value
    }

}


impl Tree {

    fn from_atom(atom: Atom) -> Tree {
        let node = ArcIntern::new(TreeNode::Atom(atom));
        Tree { node }
    }

    fn from_children<I>(children: I) -> Tree
    where
        I: IntoIterator<Item = Tree>,
    {
        let children = children.into_iter().collect();
        let node = ArcIntern::new(TreeNode::Node(children));
        Tree { node }
    }

    fn children(&self) -> Vec<Tree> {
        match &*self.node {
            TreeNode::Node(children) => children.clone(),
            TreeNode::Atom(_) => vec![],
        }
    }

}


// ---------------------------------------------- Repr (__repr__)


trait Repr {

    fn repr(&self) -> String;

}


impl Repr for AtomType {

    fn repr(&self) -> String {
        self.name().clone()
    }

}


impl Repr for AtomValue {

    fn repr(&self) -> String {
        match &self {
            AtomValue::Str(val) => format!("'{}'", val),
            AtomValue::Int(val) => format!("{}", val),
        }
    }

}


impl Repr for Atom {

    fn repr(&self) -> String {
        format!("{}({})", self.atom_type().repr(), self.value().repr())
    }

}


impl Repr for Tree {

    fn repr(&self) -> String {
        match &*self.node {
            TreeNode::Atom(atom) => {
                format!("TreeAtom({})", atom.repr())
            },
            TreeNode::Node(children) => {
                let children = children
                    .into_iter()
                    .map(|x| x.repr())
                    .collect::<Vec<String>>()
                    .join(", ");
                format!("TreeNode({})", children)
            },
        }
    }

}


// -------------------------------------------- Display (__str__)


impl fmt::Display for AtomValue {

    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            AtomValue::Str(val) => write!(f, "{}", val),
            AtomValue::Int(val) => write!(f, "{}", val),
        }
    }

}


impl fmt::Display for Atom {

    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        AtomValue::fmt(self.value(), f)
    }

}


impl fmt::Display for Tree {

    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {

        match &*self.node {
            TreeNode::Atom(atom) => {
                write!(f, "{}", atom)?
            },
            TreeNode::Node(children) => {
                let head = children[0].to_string();
                let args = children[1..]
                    .into_iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<String>>()
                    .join(", ");
                write!(f, "{}({})", head, args)?
            },

        };
        Ok(())
    }

}


// --------------------------------------------- algorithms


fn topological_sort(expression: Tree, heads: bool) -> Vec<Tree> {

    let reverse_pop_head = |children: &mut Vec<Tree>| {
        children.reverse();
        if ! heads {
            children.pop(); // pop off the head (from end because reversed).
        }
    };

    let mut seen = HashSet::new();
    let mut expressions = vec![];
    let mut stack = vec![];

    let mut children = expression.children();
    reverse_pop_head(&mut children);
    stack.push((expression, children));

    while ! stack.is_empty() {
        // Pop the next expression and its unprocessed children off the stack.
        let (expression, mut children) = stack.pop().unwrap();

        // Find the first unseen child (if any).
        let mut unseen_child = None;

        while ! children.is_empty() {

            let child = children.pop().unwrap();

            if ! seen.contains(&child) {
                seen.insert(child.clone());
                unseen_child = Some(child);
                break;
            }
        }

        // Either recurse or push to output.
        match unseen_child {
            Some(child) => {
                // Push this expression and its remaining children to the stack
                // and then push the unseen child and its children to the
                // stack. When unseen child is processed we will return to the
                // current expression and its remaining children.
                let mut grand_children = child.children();
                reverse_pop_head(&mut grand_children);
                stack.push((expression, children));
                stack.push((child, grand_children));
            },
            None => {
                // There were no unseen children so push this expression to the
                // output list.
                expressions.push(expression);
            }
        }
    }

    // This should now be a list of all subexpressions in topological order so
    // that every expression appears after each of its children.
    expressions
}


// Create a Python PyTreeExpr from a rust Tree. Needed because the rust tree
// might need to become a PyTreeAtom or a PyTreeNode. Constructing an instance
// of a sub-pyclass is awkward in pyo3. Ideally we should just get rid of the
// subclasses since they don't really do anything useful here in the rust code
// but they are there to replicate the exact classes used in the Python code.
// Maybe the Python code should be changed to use a single type like Tree as
// well.

impl IntoPy<PyObject> for Tree {

    fn into_py(self, py: Python<'_>) -> PyObject {
        let initializer: PyClassInitializer<PyTreeExpr> = PyClassInitializer::from(
            PyTreeExpr { tree: self.clone() }
        );
        let object = match *self.node {
            TreeNode::Atom(_) => {
                let initializer = initializer.add_subclass(PyTreeAtom {});
                PyCell::new(py, initializer).unwrap().to_object(py)
            },
            TreeNode::Node(_) => {
                let initializer = initializer.add_subclass(PyTreeNode {});
                PyCell::new(py, initializer).unwrap().to_object(py)
            }
        };
        object
    }
}


#[pyfunction(name = "topological_sort")]
#[pyo3(signature = (expression, heads=false))]
fn topological_sort_py( expression: PyTreeExpr, heads: bool) -> Vec<Tree> {
    topological_sort(expression.tree, heads)
}


// --------------------------------------------- AtomValue <--> Python

// We don't implement FromPyObject because we need to know whether we are
// expecting str or int.
impl AtomValue {

    fn from_pyobject(type_name: &str, value: PyObject, py: Python <'_>) -> PyResult<AtomValue> {
        match type_name {
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


impl AtomType {

    fn pyatom_from_pyobject(&self, value: PyObject, py: Python <'_>) -> PyResult<PyAtom> {
        let value = AtomValue::from_pyobject(self.type_name(), value, py)?;
        Ok(PyAtom::new(self.clone(), value))
    }

}


// ------------------------------------------ PyAtom/PyAtomType methods


impl PyAtom {

    fn new(atom_type: AtomType, value: AtomValue) -> PyAtom {
        PyAtom::from_atom(Atom::new(atom_type, value))
    }

    fn from_atom(atom: Atom) -> PyAtom {
        PyAtom { atom }
    }

    fn atom_value(&self) -> &AtomValue {
        &self.atom.value()
    }

    fn get_atom_type(&self) -> &AtomType {
        &self.atom.atom_type()
    }

}


// We need to keep a mapping from Rust AtomType structs to corresponding Python
// types because PyAtomType is constructed from Python as e.g.
// AtomType('Integer', int). We should *not* store a reference to the Python
// type in the Rust PyAtomType struct because we will sometimes need to
// construct a PyAtomType from an AtomType. We do not want to store Py
// references in the AtomType struct because we want to avoid Gil-locked
// reference counting for Atom and AtomType in the plain Rust code. Instead
// every time a PyAtomType is created we will store a mapping in this global
// static variable. Ideally this would not be static but it is not clear how
// else to do it in PyO3.

static ATOM_TYPE_MAP: GILOnceCell<Mutex<HashMap<AtomType, Py<PyType>>>> = GILOnceCell::new();


fn python_type_map_add(atom_type: &AtomType, python_type: &PyType, py: Python<'_>) {

    let mut map = ATOM_TYPE_MAP
        .get_or_init(py, || Mutex::new(HashMap::new()))
        .lock()
        .unwrap();

    map.insert(atom_type.clone(), python_type.into());
}


fn python_type_map_get(atom_type: &AtomType, py: Python<'_>) -> Py<PyType> {

    let map = ATOM_TYPE_MAP
        .get_or_init(py, || Mutex::new(HashMap::new()))
        .lock()
        .unwrap();

    map.get(atom_type).unwrap().clone()
}


impl PyAtomType {

    fn new(name: &str, type_name: &str) -> Self {
        PyAtomType::from_atom_type(AtomType::new(name, type_name))
    }

    fn from_atom_type(atom_type: AtomType) -> PyAtomType {
        PyAtomType { atom_type }
    }

    fn name(&self) -> &String {
        &self.atom_type.name()
    }

}


#[pymethods]
impl PyAtom {

    fn __repr__(&self) -> String {
        format!("{}({})", self.atom_type().name(), self.atom_value().repr())
    }

    fn __str__(&self) -> String {
        format!("{}", &self.atom)
    }

    #[getter]
    fn atom_type(&self) -> PyAtomType {
        PyAtomType::from_atom_type(self.get_atom_type().clone())
    }

    #[getter]
    fn value(&self, py: Python<'_>) -> PyObject {
        self.atom_value().clone().into_py(py)
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
        self.hash(&mut hasher);
        hasher.finish()
    }
}


#[pymethods]
impl PyAtomType {

    #[new]
    fn py_new(name: String, py_type_object: &PyType, py: Python<'_>) -> PyResult<Self> {
        match py_type_object.name()? {
            type_name @ ("int" | "str") => {
                let py_atom_type = PyAtomType::new(&name, type_name);
                // Add this to the type map for python_type property
                python_type_map_add(&py_atom_type.atom_type, py_type_object, py);
                Ok(py_atom_type)
            },
            _ => {
                Err(PyValueError::new_err("type_name must be int or str."))
            },
        }
    }

    // This is what the Python code calls the attribute but it would better to
    // change that to python_type there.
    #[getter]
    fn typ<'a>(&'a self, py: Python<'a>) -> PyObject {
        self.python_type(py)
    }

    // Retrieves the Python type from the global type map
    #[getter]
    fn python_type<'a>(&'a self, py: Python<'a>) -> PyObject {
        python_type_map_get(&self.atom_type, py).into()
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
        self.hash(&mut hasher);
        hasher.finish()
    }

    fn __repr__(&self) -> &String {
        self.name()
    }

    fn __call__(&self, value: PyObject, py: Python<'_>) -> PyResult<PyAtom> {
        self.atom_type.pyatom_from_pyobject(value, py)
    }
}


impl PyTreeExpr {

    fn from_pyatom(atom: PyAtom) -> PyTreeExpr {
        PyTreeExpr { tree: Tree::from_atom(atom.atom) }
    }

}


impl ToPyObject for PyTreeExpr {

    fn to_object(&self, py: Python<'_>) -> PyObject {
        self.clone().into_py(py)
    }

}


// -------------------------------------------------- PyTreeExpr methods.


#[pymethods]
impl PyTreeExpr {

    #[pyo3(signature = (*args))]
    fn __call__(&self, args: &PyTuple, py: Python<'_>) -> PyResult<PyObject> {
        let args: Vec<PyTreeExpr> = args.extract()?;
        let args = args.into_iter().map(|x| x.tree);
        let children = once(self.tree.clone()).chain(args);
        let tree = Tree::from_children(children.map(|x| x.clone()));
        Ok(tree.into_py(py))
    }

    #[getter]
    fn children<'a>(&'a self, py: Python<'a>) -> PyResult<&PyTuple> {

        let children: Vec<PyObject> = match &*self.tree.node {
            TreeNode::Atom(_) => {
                vec![]
            },
            TreeNode::Node(children) => {
                children.into_iter().map(|x| x.clone().into_py(py)).collect()
            },
        };

        let children = PyTuple::new(py, children);
        Ok(children)
    }

    #[getter]
    fn value(&self) -> PyResult<PyAtom> {
        match &*self.tree.node {
            TreeNode::Atom(atom) => Ok(PyAtom::from_atom(atom.clone())),
            TreeNode::Node(_) => Err(PyErr::new::<PyTypeError, _>("bad atom")),
        }
    }

    fn __repr__(&self) -> String {
        self.tree.repr()
    }

    fn __str__(&self) -> String {
        format!("{}", self.tree)
    }

    fn __richcmp__(&self, other: &Self, op: CompareOp, py: Python<'_>) -> PyObject {
        match op {
            CompareOp::Eq => (self.tree == other.tree).into_py(py),
            CompareOp::Ne => (self.tree != other.tree).into_py(py),
            _ => py.NotImplemented(),
        }
    }

    fn __hash__(&self) -> u64 {
        let mut hasher = DefaultHasher::new();
        self.hash(&mut hasher);
        hasher.finish()
    }

}


#[pymethods]
impl PyTreeAtom {

    #[new]
    fn py_new(atom: PyAtom) -> (Self, PyTreeExpr) {
        (PyTreeAtom {}, PyTreeExpr::from_pyatom(atom))
    }

}


#[pymethods]
impl PyTreeNode {

    #[new]
    #[pyo3(signature = (*args))]
    fn py_new(args: &PyTuple) -> PyResult<(Self, PyTreeExpr)> {
        let children: Vec<PyTreeExpr> = args.extract()?;
        let children = children.into_iter().map(|x| x.tree);
        let tree = Tree::from_children(children);
        Ok((PyTreeNode {}, PyTreeExpr { tree }))
    }

}


// ------------------------------- Initialise the module object.


#[pymodule]
fn rust_protosym(_py: Python, m: &PyModule) -> PyResult<()> {
    m.add_class::<PyAtomType>()?;
    m.add_class::<PyAtom>()?;
    m.add_class::<PyTreeExpr>()?;
    m.add_class::<PyTreeAtom>()?;
    m.add_class::<PyTreeNode>()?;
    m.add_function(wrap_pyfunction!(topological_sort_py, m)?)?;
    Ok(())
}
