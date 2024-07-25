CREATE FUNCTION str_count(val STRING, find CHAR) RETURNS INTEGER LANGUAGE R {
  nchar(val) - nchar(gsub(find, '', val))
};
