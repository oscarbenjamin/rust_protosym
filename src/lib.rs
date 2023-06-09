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
//    Tree
//    Tr (alias for Tree.atom)
//    ForwardGraph
//    SubsFunc
//    DiffProperties
//
// Functions
//    topological_sort
//    topological_split
//    forward_graph
//    diff_forward
//
// For now the Rust code can only handle Atoms with an internal value that is a
// string or an integer and also for integers it only uses 64 bit signed
// integers. The intention is certainly to improve that.
//

use std::collections::hash_map::DefaultHasher;
use std::collections::HashMap;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::iter::once;
use std::sync::{Arc, Mutex};

use pyo3::class::basic::CompareOp;
use pyo3::exceptions::PyTypeError;
use pyo3::exceptions::PyValueError;
use pyo3::once_cell::GILOnceCell;
use pyo3::prelude::*;
use pyo3::types::PyTuple;
use pyo3::types::PyType;

use fnv;
use internment;

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

type InternedTreeNode = internment::ArcIntern<TreeNode>;

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
    node: InternedTreeNode,
}

type TreeSet = fnv::FnvHashSet<Tree>;
type TreeIndexMap = fnv::FnvHashMap<Tree, usize>;

#[derive(PartialEq, Eq, Clone)]
struct ForwardGraph {
    atoms: Vec<Tree>,
    heads: TreeSet,
    operations: Vec<(Tree, Vec<usize>)>,
}

#[derive(Clone)]
struct SubsFunc {
    nargs: usize,
    atoms: Vec<Tree>,
    operations: Vec<Vec<usize>>,
}

type DiffRuleMap = fnv::FnvHashMap<(Tree, usize), SubsFunc>;

struct DiffProperties {
    zero: Tree,
    one: Tree,
    add: Tree,
    mul: Tree,
    distributive: TreeSet,
    diff_rules: DiffRuleMap,
}

// ------------------- Structs for the Python objects

#[pyclass(name = "AtomType")]
#[derive(PartialEq, Eq, Hash, Clone)]
struct PyAtomType {
    atom_type: AtomType,
}

#[pyclass(name = "Atom")]
#[derive(PartialEq, Eq, Hash, Clone)]
struct PyAtom {
    atom: Atom,
}

#[pyclass(name = "Tree", subclass)]
#[derive(PartialEq, Eq, Hash, Clone)]
struct PyTree {
    tree: Tree,
}

#[pyclass(name = "ForwardGraph")]
#[derive(PartialEq, Eq, Clone)]
struct PyForwardGraph {
    forward_graph: ForwardGraph,
}

#[pyclass(name = "SubsFunc")]
#[derive(Clone)]
struct PySubsFunc {
    subs_func: SubsFunc,
}

#[pyclass(name = "DiffProperties")]
struct PyDiffProperties {
    diff_properties: DiffProperties,
}

// ---------------------------------------- Construct Interned Tree

impl AtomType {
    fn new(name: &str, type_name: &str) -> AtomType {
        let name = name.to_string();
        let type_name = type_name.to_string();
        let _atom_type = Arc::new(_AtomType { name, type_name });
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
        let node = InternedTreeNode::new(TreeNode::Atom(atom));
        Tree { node }
    }

    fn from_children<I>(children: I) -> Tree
    where
        I: IntoIterator<Item = Tree>,
    {
        let children = children.into_iter().collect();
        let node = InternedTreeNode::new(TreeNode::Node(children));
        Tree { node }
    }

    fn call<I>(&self, args: I) -> Tree
    where
        I: IntoIterator<Item = Tree>,
    {
        Tree::from_children(once(self.clone()).chain(args))
    }

    fn children(&self) -> Vec<Tree> {
        match &*self.node {
            TreeNode::Node(children) => children.clone(),
            TreeNode::Atom(_) => vec![],
        }
    }
}

impl ForwardGraph {
    fn new(atoms: Vec<Tree>, heads: TreeSet, operations: Vec<(Tree, Vec<usize>)>) -> ForwardGraph {
        ForwardGraph {
            atoms,
            heads,
            operations,
        }
    }
}

impl SubsFunc {
    fn new(nargs: usize, atoms: Vec<Tree>, operations: Vec<Vec<usize>>) -> SubsFunc {
        SubsFunc {
            nargs,
            atoms,
            operations,
        }
    }

    fn from_expr_args(expr: Tree, args: Vec<Tree>) -> SubsFunc {
        let (nargs, atoms, operations) = make_function_evaluator(expr, args);
        SubsFunc::new(nargs, atoms, operations)
    }

    fn call<I>(&self, args: I) -> Tree
    where
        I: IntoIterator<Item = Tree>,
    {
        let mut stack: Vec<Tree> = args.into_iter().collect();
        stack.extend(self.atoms.clone());

        for indices in self.operations.clone() {
            let children = indices.iter().map(|x| stack[x.clone()].clone());
            stack.push(Tree::from_children(children));
        }

        stack.last().unwrap().clone()
    }
}

impl DiffProperties {
    fn from_ring_props(zero: Tree, one: Tree, add: Tree, mul: Tree) -> DiffProperties {
        let distributive = TreeSet::default();
        let diff_rules = DiffRuleMap::default();
        DiffProperties {
            zero,
            one,
            add,
            mul,
            distributive,
            diff_rules,
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
                format!("Tr({})", atom.repr())
            }
            TreeNode::Node(children) => {
                let children = children
                    .into_iter()
                    .map(|x| x.repr())
                    .collect::<Vec<String>>()
                    .join(", ");
                format!("Tree({})", children)
            }
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
            TreeNode::Atom(atom) => write!(f, "{}", atom)?,
            TreeNode::Node(children) => {
                let head = children[0].to_string();
                let args = children[1..]
                    .into_iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<String>>()
                    .join(", ");
                write!(f, "{}({})", head, args)?
            }
        };
        Ok(())
    }
}

// --------------------------------------------- algorithms

fn topological_sort(expression: Tree, heads: bool, exclude: Option<TreeSet>) -> Vec<Tree> {
    let reverse_pop_head = |children: &mut Vec<Tree>| {
        children.reverse();
        if !heads {
            children.pop(); // pop off the head (from end because reversed).
        }
    };

    let mut seen = TreeSet::default();
    if let Some(exclude) = exclude {
        seen.extend(exclude);
    }

    let mut expressions = vec![];
    let mut stack = vec![];

    let mut children = expression.children();
    reverse_pop_head(&mut children);

    if !seen.contains(&expression) {
        stack.push((expression, children));
    }

    while !stack.is_empty() {
        // Pop the next expression and its unprocessed children off the stack.
        let (expression, mut children) = stack.pop().unwrap();

        // Find the first unseen child (if any).
        let mut unseen_child = None;

        while !children.is_empty() {
            let child = children.pop().unwrap();

            if !seen.contains(&child) {
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
            }
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

fn topological_split(expr: Tree) -> (Vec<Tree>, TreeSet, Vec<Tree>) {
    let subexpressions = topological_sort(expr, false, None);

    let mut atoms = vec![];
    let mut heads = TreeSet::default();
    let mut nodes = vec![];

    for subexpr in subexpressions {
        match &*subexpr.node {
            TreeNode::Atom(_) => atoms.push(subexpr),
            TreeNode::Node(children) => {
                heads.insert(children[0].clone());
                nodes.push(subexpr);
            }
        }
    }
    (atoms, heads, nodes)
}

fn forward_graph(expr: Tree) -> ForwardGraph {
    let (atoms, heads, nodes) = topological_split(expr);

    let num_atoms = atoms.len();

    let mut operations = Vec::with_capacity(atoms.len() + nodes.len());
    let mut indices = TreeIndexMap::default();

    for (index, atom) in atoms.iter().enumerate() {
        indices.insert(atom.clone(), index);
    }

    for (index, subexpr) in nodes.into_iter().enumerate() {
        let children = subexpr.children();
        let head = children[0].clone();
        let arg_indices: Vec<usize> = children[1..]
            .into_iter()
            .map(|x| indices.get(x).unwrap().clone())
            .collect();
        operations.push((head, arg_indices));
        indices.insert(subexpr, index + num_atoms);
    }

    ForwardGraph::new(atoms, heads, operations)
}

fn make_function_evaluator(expr: Tree, args: Vec<Tree>) -> (usize, Vec<Tree>, Vec<Vec<usize>>) {
    let num_args = args.len();
    let args_set = TreeSet::from_iter(args.clone());

    let subexpressions = topological_sort(expr.clone(), true, Some(args_set));

    let mut atoms = Vec::new();
    let mut nodes = Vec::new();
    let mut has_args = TreeSet::from_iter(args.clone());
    let mut node_children = TreeSet::default();

    for subexpr in subexpressions {
        let children = subexpr.children();
        if children.is_empty() {
            atoms.push(subexpr);
        } else {
            let children_set = TreeSet::from_iter(children);
            if children_set.is_disjoint(&has_args) {
                // This node does not have args among its (in)direct children. We treat it as
                // atomic for the purposes of rebuilding the expression.
                atoms.push(subexpr);
            } else {
                // This does have args so we cannot rebuild it ahead of time. Push to the nodes
                // stack.
                has_args.insert(subexpr.clone());
                node_children.extend(children_set);
                nodes.push(subexpr);
            }
        }
    }

    if !atoms.is_empty() && nodes.is_empty() {
        // We get here if expr does not contain args.
        atoms = vec![expr];
    } else {
        // Prune atoms that are not a child of any node.
        atoms = atoms
            .into_iter()
            .filter(|x| node_children.contains(x))
            .collect();
    }

    let num_args_atoms = num_args + atoms.len();
    let args_atoms = args.iter().chain(atoms.iter()).map(|x| x.clone());
    let mut indices: TreeIndexMap = args_atoms.zip(0..num_args_atoms).collect();

    let mut operations = vec![];
    for (index, node) in nodes.into_iter().enumerate() {
        let child_indices = node
            .children()
            .iter()
            .map(|x| indices.get(&x).unwrap().clone())
            .collect();
        operations.push(child_indices);
        indices.insert(node, index + num_args_atoms);
    }

    (num_args, atoms, operations)
}

fn diff_forward(expression: Tree, sym: Tree, prop: &DiffProperties) -> Tree {
    let graph = forward_graph(expression);

    let mut stack = graph.atoms.clone();
    let mut diff_stack: Vec<Tree> = stack
        .clone()
        .into_iter()
        .map(|x| {
            if x == sym {
                prop.one.clone()
            } else {
                prop.zero.clone()
            }
        })
        .collect();

    for (func, indices) in graph.operations {
        let mut args = Vec::with_capacity(indices.len());
        let mut diff_args = Vec::with_capacity(indices.len());
        for i in indices {
            args.push(stack[i].clone());
            diff_args.push(diff_stack[i].clone());
        }

        let mut diff_terms = Vec::with_capacity(diff_args.len());

        if prop.distributive.contains(&func) {
            // Distributive rule f(x, y)' = f(x', y')
            diff_terms.push(func.call(diff_args));
        } else if diff_args.clone().into_iter().all(|a| a == prop.zero) {
            // This expression does not depend on sym at all. This will return zero but it is not
            // clear that the expression is a number so zero may be incorrect.
            ();
        } else if func == prop.add {
            diff_terms.extend(diff_args.into_iter().filter(|a| *a != prop.zero));
        } else if func == prop.mul {
            // Product rule
            for (i, da) in diff_args.into_iter().enumerate() {
                if da != prop.zero {
                    let mut new_args = args.clone();
                    new_args[i] = da;
                    let term = prop.mul.call(new_args);
                    diff_terms.push(term);
                }
            }
        } else {
            // Chain rule
            for (i, diff_arg) in diff_args.into_iter().enumerate() {
                if diff_arg != prop.zero {
                    let pdiff = prop.diff_rules.get(&(func.clone(), i)).unwrap();
                    let mut diff_term = pdiff.call(args.clone());
                    if diff_arg != prop.one {
                        diff_term = prop.mul.call(vec![diff_term, diff_arg]);
                    }
                    diff_terms.push(diff_term);
                }
            }
        }

        let expr = func.call(args);

        // Peel off zero or one arg Adds
        let derivative = if diff_terms.len() == 0 {
            prop.zero.clone()
        } else if diff_terms.len() == 1 {
            diff_terms[0].clone()
        } else {
            prop.add.call(diff_terms)
        };

        stack.push(expr);
        diff_stack.push(derivative);
    }

    diff_stack.last().unwrap().clone()
}

// --------------------------------------------- AtomValue <--> Python

// We don't implement FromPyObject because we need to know whether we are
// expecting str or int.
impl AtomValue {
    fn from_pyobject(type_name: &str, value: PyObject, py: Python<'_>) -> PyResult<AtomValue> {
        match type_name {
            "int" => Ok(AtomValue::Int(value.extract(py)?)),
            "str" => Ok(AtomValue::Str(value.extract(py)?)),
            _ => Err(PyTypeError::new_err("bad internal type.")),
        }
    }
}

impl IntoPy<PyObject> for AtomValue {
    fn into_py(self, py: Python<'_>) -> PyObject {
        match self {
            Self::Str(value) => value.into_py(py),
            Self::Int(value) => value.into_py(py),
        }
    }
}

impl AtomType {
    fn pyatom_from_pyobject(&self, value: PyObject, py: Python<'_>) -> PyResult<PyAtom> {
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
            }
            _ => Err(PyValueError::new_err("type_name must be int or str.")),
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

impl IntoPy<PyObject> for Tree {
    fn into_py(self, py: Python<'_>) -> PyObject {
        PyTree::from_tree(self).into_py(py)
    }
}

impl<'py> FromPyObject<'py> for Tree {
    fn extract(ob: &'py PyAny) -> PyResult<Self> {
        let ob: PyTree = ob.extract()?;
        Ok(ob.tree)
    }
}

impl PyTree {
    fn from_tree(tree: Tree) -> PyTree {
        PyTree { tree }
    }

    fn from_pyatom(atom: PyAtom) -> PyTree {
        PyTree::from_tree(Tree::from_atom(atom.atom))
    }

    fn from_children(children: Vec<PyTree>) -> PyTree {
        let children = children.into_iter().map(|x| x.tree);
        PyTree::from_tree(Tree::from_children(children))
    }
}

impl ToPyObject for PyTree {
    fn to_object(&self, py: Python<'_>) -> PyObject {
        self.clone().into_py(py)
    }
}

// -------------------------------------------------- PyTree methods.

#[pymethods]
impl PyTree {
    #[new]
    #[pyo3(signature = (*args))]
    fn py_new(args: &PyTuple) -> PyResult<Self> {
        let children: Vec<PyTree> = args.extract()?;
        Ok(PyTree::from_children(children))
    }

    #[staticmethod]
    fn atom(atom: PyAtom) -> PyTree {
        PyTree::from_pyatom(atom)
    }

    #[pyo3(signature = (*args))]
    fn __call__(&self, args: &PyTuple, py: Python<'_>) -> PyResult<PyObject> {
        let args: Vec<PyTree> = args.extract()?;
        let args = args.into_iter().map(|x| x.tree.clone());
        let tree = self.tree.call(args);
        Ok(tree.into_py(py))
    }

    #[getter]
    fn children<'a>(&'a self, py: Python<'a>) -> PyResult<&PyTuple> {
        let children: Vec<PyObject> = match &*self.tree.node {
            TreeNode::Atom(_) => {
                vec![]
            }
            TreeNode::Node(children) => children
                .into_iter()
                .map(|x| x.clone().into_py(py))
                .collect(),
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

#[pyfunction]
#[pyo3(name = "Tr")]
fn tree_atom(atom: PyAtom) -> PyTree {
    PyTree::atom(atom)
}

impl PyForwardGraph {
    fn from_forward_graph(forward_graph: ForwardGraph) -> PyForwardGraph {
        PyForwardGraph { forward_graph }
    }
}

#[pymethods]
impl PyForwardGraph {
    #[new]
    fn new(
        atoms: Vec<Tree>,
        heads: TreeSet,
        operations: Vec<(Tree, Vec<usize>)>,
    ) -> PyForwardGraph {
        let graph = ForwardGraph::new(atoms, heads, operations);
        PyForwardGraph::from_forward_graph(graph)
    }

    #[getter]
    fn atoms(&self) -> Vec<Tree> {
        self.forward_graph.atoms.clone()
    }

    #[getter]
    fn heads(&self) -> TreeSet {
        self.forward_graph.heads.clone()
    }

    #[getter]
    fn operations(&self) -> Vec<(Tree, Vec<usize>)> {
        self.forward_graph.operations.clone()
    }

    fn __richcmp__(&self, other: &Self, op: CompareOp, py: Python<'_>) -> PyObject {
        match op {
            CompareOp::Eq => (self.forward_graph == other.forward_graph).into_py(py),
            CompareOp::Ne => (self.forward_graph != other.forward_graph).into_py(py),
            _ => py.NotImplemented(),
        }
    }
}

impl PySubsFunc {
    fn from_subs_func(subs_func: SubsFunc) -> PySubsFunc {
        PySubsFunc { subs_func }
    }
}

impl IntoPy<PyObject> for SubsFunc {
    fn into_py(self, py: Python<'_>) -> PyObject {
        PySubsFunc::from_subs_func(self).into_py(py)
    }
}

#[pymethods]
impl PySubsFunc {
    #[new]
    fn py_new(expr: Tree, args: Vec<Tree>) -> PySubsFunc {
        let subs_func = SubsFunc::from_expr_args(expr, args);
        PySubsFunc::from_subs_func(subs_func)
    }

    #[pyo3(signature = (*args))]
    fn __call__(&self, args: &PyTuple, py: Python<'_>) -> PyResult<PyObject> {
        if args.len() != self.subs_func.nargs {
            return Err(PyTypeError::new_err("Wrong number of arguments"));
        }
        let args: Vec<PyTree> = args.extract()?;
        let args = args.into_iter().map(|x| x.tree);
        let result = self.subs_func.call(args);
        Ok(result.into_py(py))
    }

    #[getter]
    fn nargs(&self) -> usize {
        self.subs_func.nargs
    }

    #[getter]
    fn atoms(&self) -> Vec<Tree> {
        self.subs_func.atoms.clone()
    }

    #[getter]
    fn operations(&self) -> Vec<Vec<usize>> {
        self.subs_func.operations.clone()
    }
}

impl PyDiffProperties {
    fn from_diff_properties(diff_properties: DiffProperties) -> PyDiffProperties {
        PyDiffProperties { diff_properties }
    }
}

#[pymethods]
impl PyDiffProperties {
    #[new]
    fn py_new(zero: Tree, one: Tree, add: Tree, mul: Tree) -> PyDiffProperties {
        let diff_properties = DiffProperties::from_ring_props(zero, one, add, mul);
        PyDiffProperties::from_diff_properties(diff_properties)
    }

    fn add_distributive(&mut self, head: Tree) {
        self.diff_properties.distributive.insert(head);
    }

    fn add_diff_rule(&mut self, head: Tree, argnum: usize, func: PySubsFunc) {
        self.diff_properties
            .diff_rules
            .insert((head, argnum), func.subs_func);
    }

    #[getter]
    fn zero(&self) -> Tree {
        self.diff_properties.zero.clone()
    }

    #[getter]
    fn one(&self) -> Tree {
        self.diff_properties.one.clone()
    }

    #[getter]
    fn add(&self) -> Tree {
        self.diff_properties.add.clone()
    }

    #[getter]
    fn mul(&self) -> Tree {
        self.diff_properties.mul.clone()
    }

    #[getter]
    fn distributive(&self) -> TreeSet {
        self.diff_properties.distributive.clone()
    }

    #[getter]
    fn diff_rules(&self) -> DiffRuleMap {
        self.diff_properties.diff_rules.clone()
    }
}

#[pyfunction(name = "topological_sort")]
#[pyo3(signature = (expression, heads=false, exclude=None))]
fn topological_sort_py(expression: PyTree, heads: bool, exclude: Option<TreeSet>) -> Vec<Tree> {
    topological_sort(expression.tree, heads, exclude)
}

#[pyfunction(name = "topological_split")]
fn topological_split_py(expression: PyTree) -> (Vec<Tree>, TreeSet, Vec<Tree>) {
    topological_split(expression.tree)
}

#[pyfunction(name = "forward_graph")]
fn forward_graph_py(expression: PyTree) -> PyForwardGraph {
    let graph = forward_graph(expression.tree);
    PyForwardGraph::from_forward_graph(graph)
}

#[pyfunction(name = "diff_forward")]
fn diff_forward_py(expression: Tree, sym: Tree, prop: &PyDiffProperties) -> Tree {
    diff_forward(expression, sym, &prop.diff_properties)
}

// ------------------------------- Initialise the module object.

#[pymodule]
fn rust_protosym(_py: Python, m: &PyModule) -> PyResult<()> {
    m.add_class::<PyAtomType>()?;
    m.add_class::<PyAtom>()?;
    m.add_class::<PyTree>()?;
    m.add_function(wrap_pyfunction!(tree_atom, m)?)?; // Tr
    m.add_class::<PyForwardGraph>()?;
    m.add_class::<PySubsFunc>()?;
    m.add_class::<PyDiffProperties>()?;
    m.add_function(wrap_pyfunction!(topological_sort_py, m)?)?;
    m.add_function(wrap_pyfunction!(topological_split_py, m)?)?;
    m.add_function(wrap_pyfunction!(forward_graph_py, m)?)?;
    m.add_function(wrap_pyfunction!(diff_forward_py, m)?)?;
    Ok(())
}
