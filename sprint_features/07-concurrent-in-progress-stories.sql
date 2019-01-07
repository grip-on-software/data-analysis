SELECT ${f(join_cols, "updatedata")}, updatedata.key,
	MAX(updatedata.story_points) AS story_points,
	COUNT(DISTINCT startdata.issue_id) AS num
FROM (
	SELECT ${f(join_cols, "issue")}, ${t("issue")}.issue_id,
		${s(issue_key)} AS key, MAX(${s(story_points)}) AS story_points,
		MIN(${t("issue")}.updated) AS "date"
	FROM gros.${t("issue")}
	WHERE ${s(issue_story)} AND ${s(issue_in_progress)}
	GROUP BY ${f(join_cols, "issue")}, ${t("issue")}.issue_id, ${f("issue_key")}
    UNION ALL
    SELECT ${f(join_cols, "issue")}, ${t("issue")}.issue_id,
		${s(issue_key)} AS key, MAX(${s(story_points)}) AS story_points,
		MIN(${t("issue")}.updated) AS "date"
	FROM gros.${t("issue")}
	WHERE ${s(issue_story)} AND ${s(issue_done)}
	GROUP BY ${f(join_cols, "issue")}, ${t("issue")}.issue_id, ${f("issue_key")}
) AS updatedata, (
	SELECT ${f(join_cols, "issue")}, ${t("issue")}.issue_id,
		MIN(${t("issue")}.updated) AS start_date
	FROM gros.${t("issue")}
	WHERE ${s(issue_story)} AND ${s(issue_in_progress)}
	GROUP BY ${f(join_cols, "issue")}, ${t("issue")}.issue_id
) AS startdata, (
	SELECT ${f(join_cols, "issue")}, ${t("issue")}.issue_id,
		MIN(${t("issue")}.updated) AS end_date
	FROM gros.${t("issue")}
	WHERE ${s(issue_story)} AND ${s(issue_done)}
	GROUP BY ${f(join_cols, "issue")}, ${t("issue")}.issue_id
) AS enddata
WHERE ${j(join_cols, "startdata", "enddata")}
AND ${j(join_cols, "updatedata", "startdata")}
AND updatedata.issue_id <> startdata.issue_id
AND "date" BETWEEN start_date and end_date
GROUP BY ${f(join_cols, "updatedata")}, updatedata.key
