SELECT ${f(join_cols, "issue")}, COUNT(*) AS num_links
FROM gros.${t("issue")}
LEFT JOIN gros.issuelink
ON ${t("issue")}.key = issuelink.from_key
AND COALESCE(issuelink.start_date, ${t("issue")}.updated) <= ${t("issue")}.updated
WHERE ${s(issue_story)}
${s(project_condition, project="issue")}
${g(join_cols, "issue")}
