SELECT ${f(join_cols, "repo")}, COUNT(*) AS num_requests
FROM gros.${t("merge_request")}
JOIN gros.${t("repo")} ON ${t("merge_request")}.repo_id = ${t("repo")}.id
${g(join_cols, "repo")}
