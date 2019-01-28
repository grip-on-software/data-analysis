-- Number of Jira updaters in sprint
SELECT ${f(join_cols, "sprint_devs")}, COUNT(*) AS number_of_jira_devs FROM (
	SELECT ${f(join_cols, "issue")}, ${t("issue")}.updated_by AS developer_id
	FROM gros.${t("issue")}
	${s(issue_join)}
	${g(join_cols, "issue", "developer_id")}
	HAVING ${f(join_cols, "issue", mask=2)} <> 0
    UNION ALL
    SELECT ${f(join_cols, "issue")}, comment.author AS developer_id
	FROM gros.comment
    JOIN gros.${t("issue")} ON ${t("issue")}.issue_id = comment.issue_id
	${s(issue_join)}
    ${g(join_cols, "issue", "developer_id")}
	HAVING ${f(join_cols, "issue", mask=2)} <> 0
) AS sprint_devs
${g(join_cols, "sprint_devs")}
