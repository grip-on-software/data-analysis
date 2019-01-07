SELECT ${f(join_cols, "issue")}, SUM(${t("issue")}.attachments) AS num_attachments FROM gros.${t("issue")},
    (SELECT ${t("issue")}.issue_id, MAX(${t("issue")}.changelog_id) AS changelog_id
    FROM gros.${t("issue")} GROUP BY ${t("issue")}.issue_id) AS maxdata
WHERE ${t("issue")}.issue_id = maxdata.issue_id AND ${t("issue")}.changelog_id = maxdata.changelog_id
AND ${t("issue")}.sprint_id <> 0
GROUP BY ${f(join_cols, "issue")}
