SELECT ${f(join_cols, "other_issue")}, COUNT(*) AS other_done_issues FROM (
	SELECT ${f(join_cols, "issue")}, ${t("issue")}.issue_id
	FROM gros.${t("issue")}
	${s(issue_join)}
    WHERE ${s(issue_other)}
    AND ${s(sprint_id, sprint="issue")} <> 0
    AND ${s(issue_done)}
	${s(project_condition, project="issue")}
    ${g(join_cols, "issue")}, ${t("issue")}.issue_id
) AS other_issue
${g(join_cols, "other_issue")}
