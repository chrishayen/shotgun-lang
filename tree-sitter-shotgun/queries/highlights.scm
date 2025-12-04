; Keywords
[
  "fn"
  "struct"
  "trait"
  "impl"
  "error"
  "return"
  "if"
  "else"
  "for"
  "in"
  "match"
  "go"
  "chan"
] @keyword

[
  "and"
  "or"
  "not"
  "wait"
] @keyword.operator

"self" @variable.builtin

; Types
(primitive_type) @type.builtin

(type_identifier) @type

(struct_definition name: (type_identifier) @type)
(trait_definition name: (type_identifier) @type)
(impl_block type: (type_identifier) @type)
(impl_block trait: (type_identifier) @type)
(method_definition type: (type_identifier) @type)
(error_definition name: (type_identifier) @type)

; Functions and methods
(function_definition name: (identifier) @function)
(method_definition name: (identifier) @function.method)
(impl_method name: (identifier) @function.method)
(trait_method name: (identifier) @function.method)

(call_expression
  (identifier) @function.call)

(call_expression
  (member_expression
    member: (identifier) @function.method.call))

; Parameters
(parameter (identifier) @variable.parameter)

; Fields
(field_definition name: (identifier) @variable.other.member)
(field_init name: (identifier) @variable.other.member)

; Variables
(variable_declaration name: (identifier) @variable)
(for_statement (identifier) @variable)

; Literals
(number) @constant.numeric
(bool_literal) @constant.builtin.boolean
(string) @string
(string_content) @string
(escape_sequence) @constant.character.escape
(interpolation) @embedded
(none_literal) @constant.builtin

"Error" @type

; Operators
[
  "::"
  "->"
  "="
  "+="
  "-="
  "*="
  "/="
  "+"
  "-"
  "*"
  "/"
  "%"
  "=="
  "!="
  "<"
  ">"
  "<="
  ">="
  "?"
] @operator

; Punctuation
[
  "("
  ")"
  "{"
  "}"
  "["
  "]"
] @punctuation.bracket

[
  ","
  ":"
  "."
] @punctuation.delimiter

; Comments
(comment) @comment
