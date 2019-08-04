
================
Programmer Guide
================

As a programmer you are looking for source code.
You found it, here it is.

.. literalinclude:: ../examples/simple.rs
    :language: rust


Key link
========

The key link between configuration file
and the executable programm that you are writen
is the configuration ``struct``.

In the demo source code is that struct named *Config*
and is *cfg* a variable of type Config.


config.yaml
===========

Nothing of ``Config`` or ``cfg`` needs to be in the YAML.
But the field names **must**.

Here the *.yaml* that goes with the above example source code.


.. literalinclude:: ../examples/config.yaml
    :language: yaml


See also
========

The documentation of `serde <https://serde.rs/>`_.

Our ``examples`` directory.

`Reverse dependencies <https://crates.io/crates/quire/reverse_dependencies>`_.
