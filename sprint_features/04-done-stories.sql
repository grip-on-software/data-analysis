-- Number of stories that are done in time
SELECT ${f(join_cols, "done_stories")}, COUNT(*) AS num_done_stories FROM 
	(SELECT DISTINCT ${f(join_cols, "issue")}, ${t("issue")}.issue_id
		FROM gros.${t("issue")} ${s(issue_join)}, gros.${t("sprint")},
		(SELECT issue_id, MAX(changelog_id) AS changelog_id FROM gros.${t("issue")} GROUP BY issue_id) AS max_issue
		WHERE ${j(join_cols, "issue", "sprint")}
		AND ${t("issue")}.issue_id = max_issue.issue_id
		AND ${s(issue_done)}
		--AND NOT ${s(issue_overdue)}
		AND ${t("issue")}.updated > ${s(sprint_open)}
		AND ${s(issue_story)}
		AND ${s(sprint_id, sprint="issue")} <> 0
		${s(project_condition, project="issue")}
	) AS done_stories
${g(join_cols, "done_stories")}
