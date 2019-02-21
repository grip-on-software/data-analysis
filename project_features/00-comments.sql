SELECT ${f(join_cols, "issue")}, COUNT(*) AS num_comments
FROM gros.comment
JOIN gros.${t("issue")} ON comment.issue_id = ${t("issue")}.issue_id AND ${t("issue")}.changelog_id = 0
${g(join_cols, "issue")}
