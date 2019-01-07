SELECT ${f(join_cols, "startdata")}, startdata.key, enddata.story_points,
	EXTRACT(day FROM enddata.end_date - startdata.start_date) AS num
FROM (
	SELECT ${f(join_cols, "issue")}, ${t("issue")}.issue_id, ${s(issue_key)},
		MIN(${t("issue")}.updated) AS start_date
	FROM gros.${t("issue")}
	WHERE ${s(issue_story_subtask)} AND ${s(issue_in_progress)}
	GROUP BY ${f(join_cols, "issue")}, ${t("issue")}.issue_id, ${f("issue_key")}
) AS startdata, (
	SELECT ${f(join_cols, "issue")}, ${t("issue")}.issue_id, ${s(issue_key)},
		MAX(${s(story_points)}) AS story_points,
		MIN(${t("issue")}.updated) AS end_date
	FROM gros.${t("issue")}
	WHERE ${s(issue_story_subtask)} AND ${s(issue_done)}
	GROUP BY ${f(join_cols, "issue")}, ${t("issue")}.issue_id, ${f("issue_key")}
) AS enddata, gros.${t("sprint")}
WHERE startdata.issue_id = enddata.issue_id
AND ${j(join_cols, "startdata", "enddata")}
AND ${j(join_cols, "startdata", "sprint")}
AND enddata.end_date > startdata.start_date
AND enddata.end_date <= ${s(sprint_close)} +
	(${t("sprint")}.end_date - ${t("sprint")}.start_date)/7
