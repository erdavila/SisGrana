package sisgrana
package investments.files

case class InputFile[A <: FileName](name: A, path: MultiLevelFilePath)
