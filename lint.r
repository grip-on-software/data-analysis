library(lintr)

linters <- with_defaults(infix_spaces_linter=NULL,
                         object_usage_linter=NULL,
                         camel_case_linter=NULL,
                         open_curly_linter(allow_single_line=TRUE),
                         closed_curly_linter(allow_single_line=TRUE),
                         single_quotes_linter=NULL)
for (filename in commandArgs(TRUE)[-1]) {
    print(lint(filename, linters=linters))
}
