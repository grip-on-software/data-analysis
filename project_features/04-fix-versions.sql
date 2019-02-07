SELECT ${f(join_cols, "fixversion")}, COUNT(*) AS num_versions
FROM gros.${t("fixversion")}
${g(join_cols, "fixversion")}
