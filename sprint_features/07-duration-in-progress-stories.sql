SELECT ${f(join_cols, "startdata")}, startdata.key, enddata.story_points,
	EXTRACT(day FROM enddata.end_date - startdata.start_date) AS num
FROM (
	SELECT ${f(join_cols, "issue")}, ${t("issue")}.issue_id,
		${s(issue_key)} AS key, MIN(${t("issue")}.updated) AS start_date
	FROM gros.${t("issue")}
	${s(issue_join)}
	WHERE ${s(issue_story_subtask)} AND ${s(issue_in_progress)}
	${g(join_cols, "issue")}, ${t("issue")}.issue_id, ${f("issue_key")}
) AS startdata, (
	SELECT ${f(join_cols, "issue")}, ${t("issue")}.issue_id,
		${s(issue_key)} AS key,
		MAX(${s(story_points)}) AS story_points,
		MIN(${t("issue")}.updated) AS end_date
	FROM gros.${t("issue")}
	${s(issue_join)}
	WHERE ${s(issue_story_subtask)} AND ${s(issue_done)}
	${g(join_cols, "issue")}, ${t("issue")}.issue_id, ${f("issue_key")}
) AS enddata, gros.${t("sprint")}
WHERE startdata.issue_id = enddata.issue_id
AND ${j(join_cols, "startdata", "enddata", source="jira")}
AND ${j(join_cols, "startdata", "sprint")}
AND enddata.end_date > startdata.start_date
AND enddata.end_date <= ${s(planned_late)} - ${s(sprint_open)} + ${s(sprint_close)}
${s(project_condition, project="startdata")}
