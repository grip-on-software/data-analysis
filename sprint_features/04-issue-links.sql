SELECT ${f(join_cols, "issue")}, COUNT(*) AS num_links
FROM gros.${t("issue")}, gros.issuelink, (
	SELECT ${t("issue")}.issue_id,
		MAX(${t("issue")}.changelog_id) AS changelog_id
	FROM gros.${t("issue")} GROUP BY ${t("issue")}.issue_id
) AS maxdata
WHERE ${t("issue")}.issue_id = maxdata.issue_id
AND ${t("issue")}.changelog_id = maxdata.changelog_id
AND ${s(issue_key)} = issuelink.from_key --OR issue.key = issuelink.to_key)
AND ${t("issue")}.sprint_id <> 0
GROUP BY ${f(join_cols, "issue")}
