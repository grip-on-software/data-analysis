SELECT DISTINCT ${f(join_cols, "issue")}, ${s(issue_key)} AS key
FROM gros.${t("issue")}
JOIN gros.${t("sprint")} ON ${j(join_cols, "issue", "sprint")}
JOIN gros.${t("issue")} AS old_issue
ON ${j(issue_next_changelog, "issue", "old_issue")}
LEFT JOIN gros.subtask ON ${t("issue")}.issue_id = subtask.id_subtask
${s(issue_join)}
WHERE ${s(issue_story)}
AND ${f(join_cols, "issue", mask=2, alias="alias", issue="old_issue")} IS NOT NULL
AND ${s(sprint_id, sprint="issue")} <> 0
AND ${s(sprint_id, sprint="issue")} <> ${s(sprint_id, sprint="issue", issue="old_issue")}
AND ${t("issue")}.updated <= ${s(planned_end)}
AND ${s(issue_not_done)}
AND subtask.id_parent IS NULL
${s(project_condition, project="issue")}
