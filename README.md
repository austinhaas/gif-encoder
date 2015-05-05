# gif-encoder

### Description

A minimal Clojure library for encoding gifs.

### Notes

This is a low-level implementation of the GIF spec. This may be useful
to users who would like to generate gifs from code, or include the
ability to render gifs from an application. It does not include any
additional features, such as image format decoding, or drawing or
compositing tools.

The Plain Text Extension is not implemented.

### Usage

See `api.cljc` for an example, user-friendly interface built-on the
low-level implementation, and `examples.cljc` for examples of creating
gifs using that API.

### Reference

* http://www.w3.org/Graphics/GIF/spec-gif89a.txt
* http://www.vurdalakov.net/misc/gif/netscape-looping-application-extension

### To do

* Optimize gifs.
* Options for handling color palettes.

### Implementation

The gif spec is encoded as a set of data structures interpreted as a
schema. Several functions are provided to implement necessary
algorithms, such as the LZW compression algorithm.

Default values are provided for some fields. No distinction is made
between default values and fixed values. And no validation is
performed. And nothing is done to prevent users from changing fixed
values. Higher-level code should provide a better interface.

#### Several data representations are employed:

1. A declarative specification of the format as a schema. This
   includes the layout for the various components of the data stream
   with labels and types and default values.

2. An instantiation of a schema, which is essentially the same as the
   schema, but with all values provided. Values are something that can
   be interpreted to match the given type as determined in the
   `field->pseudo-binary` function. Usually values are an `int`, an
   instantiation, or a sequence of one of those.

3. A pseudo-binary representation: `[bits int]`, where bits is the
   number of bits to extract from int.

4. The final binary output format.

Finally, anyone using this library will probably want to create a
user-friendly layer that provides domain-specific data structures and
functions, and then implement those by instantiating the schema and
calling the functions convert the instantions to binary. See
`api.cljc` for an example.

#### Schema Grammar

Entities are either primitive values (`bit`, `byte`, `unsigned`, and
`rgb`) or one of the compound types defined in `gif_encoder.clj` (and
in the official gif spec). Compound types are represented as vectors
of fields, and fields are vectors like `[label type default-value]`.

Type is either a single value or a vector like `[type count]`. If type
is a single value, then `count` is assumed to be 1. Count can be an `int`
or one of `:?`, `:+`, `:*` with the usual meanings.

## Thanks

Thanks to Zach Beane for [Skippy](xach.com/lisp/skippy/), which I referenced heavily while writing this library.

## License

Copyright © 2015 Austin Haas

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
