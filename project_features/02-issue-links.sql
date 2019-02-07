SELECT ${f(join_cols, "issue")}, COUNT(*) AS num_links
FROM gros.issuelink
JOIN gros.${t("issue")}
ON issuelink.from_key = ${t("issue")}.key AND ${t("issue")}.changelog_id = 0
${g(join_cols, "issue")}
