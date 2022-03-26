SELECT ${s(project_name)} AS project_name, ${f(join_cols, "sprint", mask=2, alias=F)} AS sprint_id, ${s(sprint_name)} AS sprint_name, issuedata.date AS start_date, issuedata.end_date AS date 
FROM (
	SELECT ${f(join_cols, "start_issue")}, start_issue.issue_id,
		MIN(start_issue.updated) AS date, MIN(${t("issue")}.updated) AS end_date
	FROM gros.${t("issue")} AS start_issue
	JOIN gros.${t("issue")} ON start_issue.issue_id = ${t("issue")}.issue_id AND start_issue.changelog_id < ${t("issue")}.changelog_id
	WHERE ${s(issue_story, issue="start_issue")}
	AND ${s(issue_in_progress, issue="start_issue")}
	AND ${s(issue_done)}
	${g(join_cols, "start_issue")}, start_issue.issue_id
) AS issuedata
LEFT OUTER JOIN gros.${t("sprint")}
ON ${j(join_cols, "issuedata", "sprint")}
JOIN gros.${t("project")}
ON ${j(join_cols, "issuedata", "project", mask=1)}
