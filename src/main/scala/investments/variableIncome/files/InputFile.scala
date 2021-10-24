package sisgrana
package investments.variableIncome.files

case class InputFile[A <: FileName](name: A, path: MultiLevelFilePath)
