SELECT ${f(join_cols, "backlog_sprint")}, COUNT(*) AS backlog_size FROM (
	SELECT DISTINCT ${f(join_cols, "issue")}, ${t("issue")}.issue_id
	FROM gros.${t("issue")}
	JOIN gros.${t("sprint")} ON ${j(join_cols, "issue", "sprint")}
	AND ${t("issue")}.updated < ${s(planned_end)}
) AS backlog_sprint
GROUP BY ${f(join_cols, "backlog_sprint")}
