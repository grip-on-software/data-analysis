-- Number of story watchers
SELECT ${f(join_cols, "issue")}, AVG(${t("issue")}.watchers) AS num_watchers
FROM gros.${t("issue")},
(SELECT issue_id, MAX(changelog_id) AS max_changelog_id FROM gros.${t("issue")} GROUP BY issue_id) AS maxdata
WHERE ${t("issue")}.issue_id = maxdata.issue_id
AND ${t("issue")}.changelog_id = maxdata.max_changelog_id
AND ${s(issue_story)}
AND ${t("issue")}.sprint_id <> 0
GROUP BY ${f(join_cols, "issue")}
