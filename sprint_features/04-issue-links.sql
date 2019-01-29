SELECT ${f(join_cols, "issue")}, COUNT(*) AS num_links
FROM gros.${t("issue")}
JOIN gros.issuelink
ON ${s(issue_key)} = issuelink.from_key --OR ${s(issue_key)} = issuelink.to_key
JOIN (
	SELECT ${t("issue")}.issue_id,
		MAX(${t("issue")}.changelog_id) AS changelog_id
	FROM gros.${t("issue")} GROUP BY ${t("issue")}.issue_id
) AS maxdata
ON ${j(issue_changelog, "issue", "maxdata")}
${s(issue_join)}
WHERE ${s(sprint_id, sprint="issue")} <> 0
${s(project_condition, project="issue")}
${g(join_cols, "issue")}
