SELECT ${f(join_cols, "repo")}, COUNT(DISTINCT tag_name) AS num_tags
FROM gros.${t("tag")}
JOIN gros.${t("repo")} ON ${t("tag")}.repo_id = ${t("repo")}.id
${g(join_cols, "repo")}
