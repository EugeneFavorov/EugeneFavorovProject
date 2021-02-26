PUT STREAM out-stream UNFORMATTED '  </Table>' SKIP. 
PUT STREAM out-stream UNFORMATTED '  <WorksheetOptions xmlns="urn:schemas-microsoft-com:office:excel">' SKIP.
PUT STREAM out-stream UNFORMATTED '   <Selected/>' SKIP.
PUT STREAM out-stream UNFORMATTED '   <ProtectObjects>False</ProtectObjects>' SKIP.
PUT STREAM out-stream UNFORMATTED '   <ProtectScenarios>False</ProtectScenarios>' SKIP.
PUT STREAM out-stream UNFORMATTED '  </WorksheetOptions>' SKIP.
PUT STREAM out-stream UNFORMATTED ' </Worksheet>' SKIP.
PUT STREAM out-stream UNFORMATTED '</Workbook>' SKIP.
