SELECT ${f(join_cols, "max_issue")}, COUNT(*) AS num_comments
FROM gros.comment, (
    SELECT ${f(join_cols, "issue")}, ${t("issue")}.issue_id,
		MAX(${t("issue")}.updated) AS updated
    FROM gros.${t("issue")}
	${s(issue_join)}
    WHERE ${t("issue")}.sprint_id <> 0
    ${g(join_cols, "issue")}, ${t("issue")}.issue_id
) AS max_issue
WHERE comment.issue_id = max_issue.issue_id
AND comment."date" < max_issue.updated
${g(join_cols, "max_issue")}
