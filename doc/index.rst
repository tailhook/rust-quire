Welcome to Rust-Quire
=====================

Rust-quire is a parser for yaml-based configuration files. Features:

* Structured Yaml_ configuration files
* Yaml anchors and merge (``<<``) keys
* Yaml syntax extensions [1]_: includes, list unpacking
* Rich set of tools for end users (includes, yaml anchors, variables...)

.. [1] any configuration is still valid yaml, we use Yaml tags for as an
   extension mechanism

`Github <http://github.com/tailhook/rust-quire>`_ |
`Crate <https://crates.io/crates/quire>`_

Contents:

.. toctree::
    :maxdepth: 2

    user
    programmer

.. _Yaml: http://yaml.org



