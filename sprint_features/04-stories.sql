SELECT ${f(join_cols, "stories")}, COUNT(*) AS num_stories
FROM (
	SELECT DISTINCT ${f(join_cols, "issue")}, ${t("issue")}.issue_id
	FROM gros.${t("issue")}
	JOIN gros.${t("sprint")} ON ${j(join_cols, "issue", "sprint")}
	${s(issue_join)}
	WHERE ${s(issue_story)}
	AND ${t("issue")}.updated > ${s(sprint_open)}
	AND ${t("issue")}.sprint_id <> 0
) AS stories
${g(join_cols, "stories")}
