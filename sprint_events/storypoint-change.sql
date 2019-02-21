SELECT ${s(project_name)} AS project_name, ${f(join_cols, "sprint", mask=2, alias=F)} AS sprint_id, ${t("sprint")}."name" AS sprint_name, ${t("issue")}.updated AS date
FROM gros.${t("issue")}
JOIN gros.${t("issue")} AS prev_issue
ON ${j(issue_next_changelog, "issue", "prev_issue")}
LEFT OUTER JOIN gros.${t("sprint")}
ON ${j(join_cols, "issue", "sprint")}
JOIN gros.${t("project")}
ON ${j(join_cols, "issue", "project", mask=1)}
WHERE ${t("issue")}.story_points <> prev_issue.story_points
AND ${t("issue")}.updated < ${s(sprint_close)}
ORDER BY ${f(join_cols, "project", mask=1)}, ${t("sprint")}.start_date, ${t("issue")}.updated
