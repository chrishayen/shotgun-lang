module.exports = grammar({
  name: 'shotgun',

  extras: $ => [
    /\s/,
    $.comment,
  ],

  word: $ => $.identifier,

  conflicts: $ => [
    [$.type_identifier, $.identifier],
  ],

  rules: {
    source_file: $ => repeat($._item),

    _item: $ => choice(
      $.uses_statement,
      $.struct_definition,
      $.variant_definition,
      $.trait_definition,
      $.impl_block,
      $.method_definition,
      $.function_definition,
      $.error_definition,
    ),

    // uses: std.io, std.json  OR  uses:\n  - std.io\n  - std.json
    uses_statement: $ => seq(
      'uses',
      ':',
      choice(
        $.inline_import_list,
        $.dash_import_list,
      ),
    ),

    inline_import_list: $ => seq(
      $.import_path,
      repeat(seq(',', $.import_path)),
    ),

    dash_import_list: $ => repeat1($.dash_import_item),

    dash_import_item: $ => seq('-', $.import_path),

    import_path: $ => seq(
      $.identifier,
      repeat(seq('.', $.identifier)),
    ),

    // Comments
    comment: $ => token(seq('//', /.*/)),

    // Person :: struct { ... }
    struct_definition: $ => seq(
      field('name', $.type_identifier),
      '::',
      'struct',
      '{',
      repeat($.field_definition),
      '}',
    ),

    // Option :: variant { Some { value int } None }
    variant_definition: $ => seq(
      field('name', $.type_identifier),
      optional($.type_parameters),
      '::',
      'variant',
      '{',
      repeat($.variant_case),
      '}',
    ),

    variant_case: $ => seq(
      field('name', $.type_identifier),
      optional(seq('{', repeat($.field_definition), '}')),
    ),

    type_parameters: $ => seq(
      '<',
      $.identifier,
      repeat(seq(',', $.identifier)),
      '>',
    ),

    field_definition: $ => seq(
      field('name', $.identifier),
      field('type', $._type),
    ),

    // Stringer :: trait { ... }
    trait_definition: $ => seq(
      field('name', $.type_identifier),
      '::',
      'trait',
      '{',
      repeat($.trait_method),
      '}',
    ),

    trait_method: $ => seq(
      field('name', $.identifier),
      '(',
      optional($.parameter_list),
      ')',
      optional($._type),
    ),

    // Person :: impl Stringer { ... }
    impl_block: $ => seq(
      field('type', $.type_identifier),
      '::',
      'impl',
      field('trait', $.type_identifier),
      '{',
      repeat($.impl_method),
      '}',
    ),

    impl_method: $ => seq(
      field('name', $.identifier),
      '(',
      optional($.parameter_list),
      ')',
      optional($._type),
      $.block,
    ),

    // Person :: greet(self) str { ... }
    method_definition: $ => seq(
      field('type', $.type_identifier),
      '::',
      field('name', $.identifier),
      '(',
      optional($.parameter_list),
      ')',
      optional($._return_type),
      $.block,
    ),

    // fn main { ... }
    function_definition: $ => seq(
      'fn',
      field('name', $.identifier),
      optional(seq('(', optional($.parameter_list), ')')),
      optional($._return_type),
      $.block,
    ),

    // FileNotFound :: error { ... }
    error_definition: $ => seq(
      field('name', $.type_identifier),
      '::',
      'error',
      '{',
      repeat($.field_definition),
      '}',
    ),

    _return_type: $ => seq(
      $._type,
      optional(seq('or', 'Error')),
    ),

    parameter_list: $ => seq(
      $.parameter,
      repeat(seq(',', $.parameter)),
      optional(','),
    ),

    parameter: $ => choice(
      'self',
      seq($._type, field('name', $.identifier)),
    ),

    _type: $ => choice(
      $.primitive_type,
      $.optional_type,
      $.array_type,
      $.channel_type,
      $.type_identifier,
    ),

    primitive_type: $ => choice(
      'str',
      'int',
      'bool',
      'f32',
      'f64',
      'u32',
      'u64',
    ),

    optional_type: $ => seq(
      choice($.primitive_type, $.type_identifier),
      '?',
    ),

    array_type: $ => seq(
      choice($.primitive_type, $.type_identifier),
      '[',
      ']',
    ),

    channel_type: $ => seq(
      'chan',
      $._type,
    ),

    type_identifier: $ => /[A-Z][a-zA-Z0-9_]*/,

    block: $ => seq(
      '{',
      repeat($._statement),
      '}',
    ),

    _statement: $ => choice(
      $.variable_declaration,
      $.inferred_declaration,
      $.const_declaration,
      $.return_statement,
      $.if_statement,
      $.for_statement,
      $.while_statement,
      $.match_statement,
      $.go_statement,
      $.expression_statement,
    ),

    variable_declaration: $ => seq(
      $._type,
      field('name', $.identifier),
      '=',
      $._expression,
    ),

    // name := expr (type inferred)
    inferred_declaration: $ => seq(
      field('name', $.identifier),
      ':=',
      $._expression,
    ),

    const_declaration: $ => seq(
      'const',
      field('name', $.identifier),
      '=',
      $._expression,
    ),

    expression_statement: $ => $._expression,

    return_statement: $ => prec.right(seq(
      'return',
      optional($._expression),
    )),

    if_statement: $ => prec.right(seq(
      'if',
      $._expression,
      $.block,
      optional(seq('else', choice($.if_statement, $.block))),
    )),

    for_statement: $ => seq(
      'for',
      $.identifier,
      'in',
      $._expression,
      $.block,
    ),

    while_statement: $ => seq(
      'while',
      $._expression,
      $.block,
    ),

    match_statement: $ => seq(
      'match',
      $._expression,
      '{',
      repeat($.match_arm),
      '}',
    ),

    match_arm: $ => seq(
      $.pattern,
      '->',
      choice($.block, $._expression),
    ),

    pattern: $ => seq(
      $.identifier,
      optional($.identifier),
    ),

    go_statement: $ => seq(
      'go',
      $._expression,
    ),

    _expression: $ => choice(
      $.binary_expression,
      $.unary_expression,
      $.call_expression,
      $.member_expression,
      $.or_expression,
      $.struct_literal,
      $.array_literal,
      $.identifier,
      $.number,
      $.string,
      $.char,
      $.bool_literal,
      $.none_literal,
      $.paren_expression,
      $.assignment_expression,
    ),

    paren_expression: $ => seq('(', $._expression, ')'),

    binary_expression: $ => choice(
      prec.left(1, seq($._expression, '||', $._expression)),
      prec.left(2, seq($._expression, 'and', $._expression)),
      prec.left(3, seq($._expression, choice('==', '!=', '<', '>', '<=', '>=', 'in'), $._expression)),
      prec.left(4, seq($._expression, choice('+', '-'), $._expression)),
      prec.left(5, seq($._expression, choice('*', '/', '%'), $._expression)),
    ),

    unary_expression: $ => prec(6, seq('not', $._expression)),

    or_expression: $ => prec.left(1, seq(
      $._expression,
      'or',
      choice(
        $._expression,
        $.or_return,
        $.or_error,
        $.or_wait,
      ),
    )),

    or_return: $ => prec.right(seq('return', optional($._expression))),
    or_error: $ => prec.right(seq('error', $.type_identifier, optional($.struct_literal))),
    or_wait: $ => prec.right(seq('wait', '(', $._expression, ')')),

    assignment_expression: $ => prec.right(0, seq(
      $._expression,
      choice('=', '+=', '-=', '*=', '/='),
      $._expression,
    )),

    call_expression: $ => prec(8, seq(
      $._expression,
      '(',
      optional($.argument_list),
      ')',
    )),

    member_expression: $ => prec(9, seq(
      $._expression,
      '.',
      field('member', $.identifier),
    )),

    argument_list: $ => seq(
      $._expression,
      repeat(seq(',', $._expression)),
      optional(','),
    ),

    struct_literal: $ => prec(7, seq(
      $.type_identifier,
      '{',
      optional($.field_init_list),
      '}',
    )),

    field_init_list: $ => seq(
      $.field_init,
      repeat(seq(',', $.field_init)),
      optional(','),
    ),

    field_init: $ => seq(
      field('name', $.identifier),
      ':',
      $._expression,
    ),

    array_literal: $ => seq(
      '[',
      optional(seq($._expression, repeat(seq(',', $._expression)), optional(','))),
      ']',
    ),

    identifier: $ => /[a-z_][a-zA-Z0-9_]*/,

    number: $ => /\d+(\.\d+)?/,

    string: $ => seq(
      '"',
      repeat(choice(
        $.string_content,
        $.escape_sequence,
        $.interpolation,
      )),
      '"',
    ),

    string_content: $ => /[^"\\{]+/,

    escape_sequence: $ => /\\[\\nrt"{]/,

    interpolation: $ => seq(
      '{',
      $._expression,
      '}',
    ),

    bool_literal: $ => choice('true', 'false'),

    none_literal: $ => 'none',

    char: $ => seq(
      "'",
      choice(
        $.char_content,
        $.char_escape_sequence,
      ),
      "'",
    ),

    char_content: $ => /[^'\\]/,

    char_escape_sequence: $ => /\\[\\nrt'0]/,
  }
});
