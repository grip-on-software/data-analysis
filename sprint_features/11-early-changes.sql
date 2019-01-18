SELECT ${f(join_cols, "max_issue")}, COUNT(*) AS num_early_changes
FROM gros.${t("issue")}
JOIN (SELECT ${f(join_cols, "issue")}, ${t("issue")}.issue_id,
	MAX(${t("issue")}.changelog_id) AS changelog_id
    FROM gros.${t("issue")}
	${s(issue_join)}
    ${g(join_cols, "issue")}, ${t("issue")}.issue_id
) AS max_issue
ON ${t("issue")}.issue_id = max_issue.issue_id
AND ${t("issue")}.changelog_id <= max_issue.changelog_id
JOIN gros.${t("sprint")}
ON ${j(join_cols, "max_issue", "sprint")}
${s(issue_join)}
WHERE ${t("issue")}.updated < ${s(planned_early)}
${g(join_cols, "max_issue")}
