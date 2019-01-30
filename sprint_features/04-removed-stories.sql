SELECT DISTINCT ${f(join_cols, "issue")}, ${s(issue_key)} AS key
FROM gros.${t("issue")}
JOIN gros.${t("sprint")} ON ${j(join_cols, "issue", "sprint")}
JOIN gros.${t("issue")} AS new_issue
ON ${j(issue_next_changelog, "new_issue", "issue")}
LEFT JOIN gros.subtask ON ${t("issue")}.issue_id = subtask.id_subtask
${s(issue_join)}
WHERE ${s(issue_story)}
AND ${s(sprint_id, sprint="issue")} <> 0
AND ${f(join_cols, "new_issue", mask=2, alias="alias")} IS NOT NULL
AND ${s(sprint_id, sprint="issue")} <> ${s(sprint_id, sprint="issue", issue="new_issue")}
AND new_issue.updated > ${s(planned_end)}
AND new_issue.updated <= ${s(sprint_close)} + interval '1' day
AND ${s(issue_not_done)}
AND subtask.id_parent IS NULL
${s(project_condition, project="issue")}
