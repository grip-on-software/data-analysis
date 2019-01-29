SELECT ${f(join_cols, "backlog_sprint")}, COUNT(*) AS backlog_size FROM (
	SELECT DISTINCT ${f(join_cols, "issue")}, ${t("issue")}.issue_id
	FROM gros.${t("issue")}
	${s(issue_join)}
	JOIN gros.${t("sprint")} ON ${j(join_cols, "issue", "sprint")}
	AND ${t("issue")}.updated < ${s(planned_end)}
	${s(project_condition, project="issue")}
) AS backlog_sprint
${g(join_cols, "backlog_sprint")}
