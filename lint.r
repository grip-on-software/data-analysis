# Script to perform a lint on a number of provided R scripts.
#
# Copyright 2017-2020 ICTU
# Copyright 2017-2022 Leiden University
# Copyright 2017-2023 Leon Helwerda
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

library(lintr)

linters <- linters_with_defaults(infix_spaces_linter=NULL,
                                 object_usage_linter=NULL,
                                 cyclocomp_linter=NULL,
                                 object_name_linter(styles=c("snake_case",
                                                             "symbols",
                                                             "dotted.case",
                                                             "UPPERCASE",
                                                             "SNAKE_CASE")),
                                 brace_linter(allow_single_line=TRUE),
                                 single_quotes_linter=NULL,
                                 T_and_F_symbol_linter=NULL)
lints <- lint_dir(linters=linters)
print(lints)
sarif_output(lints)
