SELECT ${f(join_cols, "backlog_ready")}, COUNT(*) AS backlog_ready_status FROM (
	SELECT DISTINCT ${f(join_cols, "issue")}, ${t("issue")}.issue_id
	FROM gros.${t("issue")}
	JOIN gros.ready_status ON issue.ready_status = ready_status.id
	JOIN gros.${t("sprint")} ON ${j(join_cols, "issue", "sprint")}
	WHERE ${t("issue")}.ready_status IS NOT NULL
	AND ${t("issue")}.updated < ${s(planned_end)}
) AS backlog_ready
GROUP BY ${f(join_cols, "backlog_ready")}
