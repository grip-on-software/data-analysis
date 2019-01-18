SELECT ${f(join_cols, "backlog_ready")}, COUNT(*) AS backlog_ready_status FROM (
	SELECT DISTINCT ${f(join_cols, "issue")}, ${t("issue")}.issue_id
	FROM gros.${t("issue")}
	${s(issue_join)}
	JOIN gros.ready_status ON ${t("issue")}.ready_status = ready_status.id
	JOIN gros.${t("sprint")} ON ${j(join_cols, "issue", "sprint")}
	WHERE ${t("issue")}.ready_status IS NOT NULL
	AND ${t("issue")}.updated < ${s(planned_end)}
) AS backlog_ready
${g(join_cols, "backlog_ready")}
