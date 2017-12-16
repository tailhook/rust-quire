.. highlight:: yaml


==========
User Guide
==========


.. _cheat-sheet:

Yaml Cheat Sheet
================

Usually YAML structure is denoted by indentation.

Here is an example of Yaml mapping (or dictinary, or associated array), used
to fill in some application's structure::

    database-connection:
        host: localhost
        port: 1878
        encoding: utf-8

The top-level thing is almost always a mapping.

Sequences (or arrays, or lists) are denoted using ``-`` at the start of each
line, for example::

    urls:
    - http://rust-lang.org
    - http://rust-quire.readthedocs.io

Note: the **space is required** after both, the colon ``:`` used to denote
a mapping and a dash ``-`` used to denote a sequence. This makes it possible
to differentiate between urls written in the last example and a mapping
example before. If you're unsure use quotes::

    urls:
    - "http://rust-lang.org"
    - 'http://rust-quire.readthedocs.io'

Values may have arbitrarily complex structure::

    databases:
      slaves:
      - host: slave1.example.com
        port: 1245
      - host: slave2.example.com
        port: 1245

There is a short form of mappings and sequences, it's markup includes
curly braces ``{}`` and square brackets ``[]`` respectively. When
this form is used, indentation is no more significant, until last brace
or bracket is closed. Example::

    databases:
      slaves: [{host: slave1, port: 1245},
        {host: slave2, port: 1245}]
    languages [ru, uk, us]


.. _quire-tricks:

Quire Tricks
============


Underscore Names
----------------

In most cases names mapping keys starting with underscore are ignored. For
example::

    host: localhost
    _port: 1778  # this is ignored
    port: 2021

In this case ``_port`` field is ignored by a validator. It can be used
to effectively comment out the piece of config (of arbitrary depth). But data
might must be valid Yaml anyway, and may include ancors.

Often, this trick is used to anchor some things in a more comfortable way, for
example this config::

    listen:
    - host: 127.0.0.1
      port: &port 1279
    - host: 172.0.0.1
      port: *port

Could be rewritten as::

    _port: &port 1278

    listen:
    - host: 127.0.0.1
      port: *port
    - host: 172.0.0.1
      port: *port

Note two things:

* The latter example is more "symmetric" in some sense, you can reorder rows
  without rewriting anchors.
* Also it's convenient to move commonly changed parts to the top of the
  configuration file if config is big


.. _integers:

Integers
--------

Integers can be of base 10, just like everybody used to. It can also start
with:

* ``0x`` to be interpreted as base 16
* ``0o`` meaning octal representation
* ``0b`` for bitmasks

Numbers can also be split into group to be easier to read using underscores:
``1_024``, ``16_777_216``.

.. _units:

Units
-----

A lot of integer values in configuration files are quite big, e.g. should be
expressed in megabytes or gigabytes. Instead of common case of making default
units of megabytes or any other arbitrary choice, quire allows to specify
order of magnitude units for every integer and floating point value. E.g::

    int1: 1M
    int2: 2k
    int3: 2 ki

Results into the following, after parsing::

   int1: 1000000
   int2: 2000
   int3: 2048

Note that there is a difference between prefixes for powers of 1024 and powers
of the 1000.

The following table summarizes all units supported:

===== ===================
Unit  Value
===== ===================
k     1000
ki    1024
M     1000000
Mi    1048576
G     1000000000
Gi    1073741824
===== ===================


Includes
========

Currently rust-quire supports a single kind of include. Includes are expanded
after parsing the yaml but config validation. It has the following
consequences:

* Both include origin and included files are valid Yaml files on it's own.
* The included data is contained at the place where directive is (unlike many
  other configuration systems where inclusion usually occurs at the top
  level of the config), but you can include at the top level of the config
  too
* All anchors are local to the file, you can't reuse anchors from an included
  file.
* Include directives can be arbirarily nested (up to the memory limit)

It depends on how API is used, but usually file name of the include directive
is expanded relative to a file that contains include (in fact relative to the
name under which file is opened in case it symlinked into multiple places)


.. _include:

Include Another Yaml
--------------------

The ``!*Include`` tag includes the contents of the file replaceing the
node that contains tag. For example:

.. code-block:: yaml

    # config.yaml
    items: !Include items.yaml

.. code-block:: yaml

    # items.yaml
    - apple
    - cherry
    - banana

Is equivalent of:

.. code-block:: yaml

   items:
   - apple
   - cherry
   - banana


.. _include-seq:

Include Sequences of Files
--------------------------

The ``!*IncludeSeq`` tag includes files matched by glob as a sequence:

.. code-block:: yaml

   items: !*IncludeSeq "fruits/*.yaml"

Can be parsed as:

.. code-block:: yaml

   items:
   - apple
   - banana
   - cherry

This depends on the exact application, but usually files returned by `glob`
are sorted in alphabetical order. All the power of globs is supported, so you
can do:

.. code-block:: yaml

   items: !*IncludeSeq "files/**/*.yaml"

Another trick is merge multiple files, each with it's own set of keys into
a single one (see map-merge_ below):

.. code-block:: yaml

   # file1.yaml
   key1: 1
   key2: 2
   # file2.yaml
   key3: 3
   key4: 4
   # main.yaml
   <<: !*IncludeSeq "configs/*.yaml"

This results into the following config:

.. code-block:: yaml

   key1: 1
   key2: 2
   key3: 3
   key4: 4

Note: merging is not recursive, i.e. top level keys are considered as a whole,
even if they are dicts.


.. _include-map:

Include Mapping from Files
--------------------------

The ``!*IncludeMap`` tag works similarly to ``!*IncludeSeq`` but requires to
mark part of a name used as a key. For example:

.. code-block:: yaml

   items: !*IncludeMap "fruits/(*).yaml"

Might result into the folowing:

.. code-block:: yaml

   items:
     apple: { color: orange }
     banana: { color: yellow }
     cherry: { color: red }

You can parenthesize any part as well as a whole path:

.. code-block:: yaml

   files: !*IncludeMap "(**/*.yaml)"


.. _map-merge:

Merging Mappings
================

We use standard YAML way for merging_ mappings. It's achieved using ``<<`` key
and either mapping or a list of mappings for the value.

.. _merging: http://yaml.org/type/merge.html

The most useful merging is with aliases. Example::

    fruits: &fruits
      apple: yes
      banana: yes
    food:
      bread: yes
      milk: yes
      <<: *fruits

Will be parsed as::

   fruits:
     apple: yes
     banana: yes
   food:
     bread: yes
     milk: yes
     apple: yes
     banana: yes


.. _seq-unpacking:

Unpacking Sequences
===================

Similarly to map merging we have a method to join two sequences, for
example::

   _wild: &wild_animals
   - tiger
   - lion
   _pets: &domestic_animals
   - cat
   - dog

   animals:
   - !*Unpack [*wild_animals, *domestic_animals]

The key thing in the example is ``!*Unpack`` tag.

Note, you always need to have a two nested lists in, i.e. this is valid:
``!*Unpack [[value]]``, but this ``!*Unpack [value]`` isn't. This is required
in order to make tags in encompassed value work.

