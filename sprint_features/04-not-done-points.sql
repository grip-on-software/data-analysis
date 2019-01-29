SELECT DISTINCT ${f(join_cols, "issue")}, ${s(issue_key)} AS key,
    ${s(story_points)} AS story_points
FROM gros.${t("issue")}
JOIN gros.${t("sprint")} ON ${j(join_cols, "issue", "sprint")}
LEFT JOIN gros.${t("issue")} AS new_issue
ON ${j(issue_next_changelog, "new_issue", "issue")}
LEFT JOIN gros.subtask ON ${t("issue")}.issue_id = subtask.id_subtask
${s(issue_join)}
WHERE ${t("issue")}.story_points IS NOT NULL
AND ${s(sprint_id, sprint="issue")} IS NOT NULL
AND (
  (new_issue.issue_id IS NULL AND ${s(sprint_close)} + interval '1' day > NOW())
  OR (
    new_issue.issue_id IS NOT NULL
	AND ${f(join_cols, "new_issue", mask=2, alias="alias")} IS NULL
    AND new_issue.updated > ${s(planned_end)}
  )
)
AND ${s(issue_not_done)}
AND subtask.id_parent IS NULL
${s(project_condition, project="issue")}
