SELECT ${f(join_cols, "issue")},
	SUM(${t("issue")}.attachments) AS num_attachments
FROM gros.${t("issue")}
JOIN (
	SELECT ${t("issue")}.issue_id, MAX(${t("issue")}.changelog_id) AS changelog_id
    FROM gros.${t("issue")} GROUP BY ${t("issue")}.issue_id
) AS maxdata
ON ${j(issue_changelog, "issue", "maxdata")}
${s(issue_join)}
WHERE ${t("issue")}.sprint_id <> 0
${g(join_cols, "issue")}
