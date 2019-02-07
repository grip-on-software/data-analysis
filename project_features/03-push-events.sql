SELECT ${f(join_cols, "repo")}, COUNT(*) AS num_push_events
FROM gros.${t("vcs_event")}
LEFT JOIN gros.${t("repo")} ON ${t("vcs_event")}.repo_id = ${t("repo")}.id
${g(join_cols, "repo")} HAVING ${f(join_cols, "repo")} IS NOT NULL
