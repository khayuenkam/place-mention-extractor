package models

case class CategoryLink(
  from: String,
  to: String,
  sortKey: String,
  sortKeyPrefix: String,
  linkType: String,
  pageTitle: Option[String]
)