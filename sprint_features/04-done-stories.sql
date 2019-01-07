-- Number of stories that are done in time
SELECT ${f(join_cols, "done_stories")}, COUNT(*) AS num_done_stories FROM 
	(SELECT DISTINCT ${f(join_cols, "issue")}, ${t("issue")}.issue_id
		FROM gros.${t("issue")}, gros.${t("sprint")},
		(SELECT issue_id, MAX(changelog_id) AS changelog_id FROM gros.${t("issue")} GROUP BY issue_id) AS max_issue
		WHERE ${j(join_cols, "issue", "sprint")}
		AND ${t("issue")}.issue_id = max_issue.issue_id
		AND ${s(issue_done)}
		--AND NOT ${s(issue_overdue)}
		AND ${t("issue")}.updated > ${s(sprint_open)}
		AND ${s(issue_story)}
		AND ${t("issue")}.sprint_id <> 0
	) AS done_stories
GROUP BY ${f(join_cols, "done_stories")}
