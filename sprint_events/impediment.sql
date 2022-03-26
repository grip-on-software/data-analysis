SELECT ${s(project_name)} AS project_name, ${f(join_cols, "sprint", mask=2, alias=F)} AS sprint_id, ${s(sprint_name)} AS sprint_name, ${t("issue")}.updated AS date
FROM gros.${t("issue")}
JOIN gros.${t("issue")} AS prev_issue
ON ${j(issue_next_changelog, "issue", "prev_issue")}
LEFT OUTER JOIN gros.${t("sprint")}
ON ${j(join_cols, "issue", "sprint")}
JOIN gros.${t("project")}
ON ${j(join_cols, "issue", "project", mask=1)}
WHERE ${t("issue")}.impediment = TRUE AND prev_issue.impediment = FALSE
