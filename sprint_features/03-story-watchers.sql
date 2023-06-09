-- Number of story watchers
SELECT ${f(join_cols, "issue")}, AVG(${t("issue")}.watchers) AS num_watchers
FROM gros.${t("issue")}
JOIN (
	SELECT issue_id, MAX(changelog_id) AS changelog_id
	FROM gros.${t("issue")} GROUP BY issue_id
) AS maxdata
ON ${j(issue_changelog, "issue", "maxdata")}
${s(issue_join)}
WHERE ${s(issue_story)}
AND ${s(sprint_id, sprint="issue")} <> 0
${s(project_condition, project="issue")}
${g(join_cols, "issue")}
