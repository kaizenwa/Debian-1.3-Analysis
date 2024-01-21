/*
EXPORTS
	AbortSystemShutdownA@4
	AbortSystemShutdownW@4
	AccessCheck@32
	AccessCheckAndAuditAlarmA@44
	AccessCheckAndAuditAlarmW@44
	AddAccessAllowedAce@16
	AddAccessDeniedAce@16
	AddAce@20
	AddAuditAccessAce@24
	AdjustTokenGroups@24
	AdjustTokenPrivileges@24
	AllocateAndInitializeSid@44
	AllocateLocallyUniqueId@4
	AreAllAccessesGranted@8
	AreAnyAccessesGranted@8
	BackupEventLogA@8
	BackupEventLogW@8
	ChangeServiceConfigA@44
	ChangeServiceConfigW@44
	ClearEventLogA@8
	ClearEventLogW@8
	CloseEventLog@4
	CloseServiceHandle@4
	ControlService@12
	CopySid@12
	CreatePrivateObjectSecurity@24
	CreateServiceA@52
	CreateServiceW@52
	DeleteAce@8
	DeleteService@4
	DeregisterEventSource@4
	DestroyPrivateObjectSecurity@4
	DuplicateToken@12
	ElfBackupEventLogFileA@8
	ElfBackupEventLogFileW@8
	ElfChangeNotify@8
	ElfClearEventLogFileA@8
	ElfClearEventLogFileW@8
	ElfCloseEventLog@4
	ElfDeregisterEventSource@4
	ElfNumberOfRecords@8
	ElfOldestRecord@8
	ElfOpenBackupEventLogA@12
	ElfOpenBackupEventLogW@12
	ElfOpenEventLogA@12
	ElfOpenEventLogW@12
	ElfReadEventLogA@28
	ElfReadEventLogW@28
	ElfRegisterEventSourceA@12
	ElfRegisterEventSourceW@12
	ElfReportEventA@48
	ElfReportEventW@48
	EnumDependentServicesA@24
	EnumDependentServicesW@24
	EnumServicesStatusA@32
	EnumServicesStatusW@32
	EqualPrefixSid@8
	EqualSid@8
	FindFirstFreeAce@8
	FreeSid@4
	GetAce@12
	GetAclInformation@16
	GetFileSecurityA@20
	GetFileSecurityW@20
	GetKernelObjectSecurity@20
	GetLengthSid@4
	GetNumberOfEventLogRecords@8
	GetOldestEventLogRecord@8
	GetPrivateObjectSecurity@20
	GetSecurityDescriptorControl@12
	GetSecurityDescriptorDacl@16
	GetSecurityDescriptorGroup@12
	GetSecurityDescriptorLength@4
	GetSecurityDescriptorOwner@12
	GetSecurityDescriptorSacl@16
	GetServiceDisplayNameA@16
	GetServiceDisplayNameW@16
	GetServiceKeyNameA@16
	GetServiceKeyNameW@16
	GetSidIdentifierAuthority@4
	GetSidLengthRequired@4
	GetSidSubAuthority@8
	GetSidSubAuthorityCount@4
	GetTokenInformation@20
DONE	GetUserNameA@8
	GetUserNameW@8
	I_ScSetServiceBitsA@20
	I_ScSetServiceBitsW@20
	ImpersonateNamedPipeClient@4
	ImpersonateSelf@4
	InitializeAcl@12
	InitializeSecurityDescriptor@8
	InitializeSid@12
	InitiateSystemShutdownA@20
	InitiateSystemShutdownW@20
	IsTextUnicode@12
	IsValidAcl@4
	IsValidSecurityDescriptor@4
	IsValidSid@4
	LockServiceDatabase@4
	LookupAccountNameA@28
	LookupAccountNameW@28
	LookupAccountSidA@28
	LookupAccountSidW@28
	LookupPrivilegeDisplayNameA@20
	LookupPrivilegeDisplayNameW@20
	LookupPrivilegeNameA@16
	LookupPrivilegeNameW@16
	LookupPrivilegeValueA@12
	LookupPrivilegeValueW@12
	LsaAddPrivilegesToAccount@8
	LsaClearAuditLog@4
	LsaClose@4
	LsaCreateAccount@16
	LsaCreateSecret@16
	LsaCreateTrustedDomain@16
	LsaDelete@4
	LsaEnumerateAccounts@20
	LsaEnumeratePrivileges@20
	LsaEnumeratePrivilegesOfAccount@8
	LsaEnumerateTrustedDomains@20
	LsaFreeMemory@4
	LsaGetQuotasForAccount@8
	LsaGetSystemAccessAccount@8
	LsaICLookupNames@28
	LsaICLookupSids@28
	LsaLookupNames@20
	LsaLookupPrivilegeDisplayName@16
	LsaLookupPrivilegeName@12
	LsaLookupPrivilegeValue@12
	LsaLookupSids@20
	LsaOpenAccount@16
	LsaOpenPolicy@16
	LsaOpenSecret@16
	LsaOpenTrustedDomain@16
	LsaQueryInfoTrustedDomain@12
	LsaQueryInformationPolicy@12
	LsaQuerySecret@20
	LsaQuerySecurityObject@12
	LsaRemovePrivilegesFromAccount@12
	LsaSetInformationPolicy@12
	LsaSetInformationTrustedDomain@12
	LsaSetQuotasForAccount@8
	LsaSetSecret@12
	LsaSetSecurityObject@12
	LsaSetSystemAccessAccount@8
	MakeAbsoluteSD@44
	MakeSelfRelativeSD@12
	MapGenericMask@8
	NotifyBootConfigStatus@4
	NotifyChangeEventLog@8
	ObjectCloseAuditAlarmA@12
	ObjectCloseAuditAlarmW@12
	ObjectOpenAuditAlarmA@48
	ObjectOpenAuditAlarmW@48
	ObjectPrivilegeAuditAlarmA@24
	ObjectPrivilegeAuditAlarmW@24
	OpenBackupEventLogA@8
	OpenBackupEventLogW@8
	OpenEventLogA@8
	OpenEventLogW@8
	OpenProcessToken@12
	OpenSCManagerA@12
	OpenSCManagerW@12
	OpenServiceA@12
	OpenServiceW@12
	OpenThreadToken@16
	PrivilegeCheck@12
	PrivilegedServiceAuditAlarmA@20
	PrivilegedServiceAuditAlarmW@20
	QueryServiceConfigA@16
	QueryServiceConfigW@16
	QueryServiceLockStatusA@16
	QueryServiceLockStatusW@16
	QueryServiceObjectSecurity@20
	QueryServiceStatus@8
	QueryWindows31FilesMigration@4
	ReadEventLogA@28
	ReadEventLogW@28
DONE	RegCloseKey@4
	RegConnectRegistryA@12
	RegConnectRegistryW@12
	RegCreateKeyA@12
DONE	RegCreateKeyExA@36
	RegCreateKeyExW@36
	RegCreateKeyW@12
DONE	RegDeleteKeyA@8
	RegDeleteKeyW@8
	RegDeleteValueA@8
	RegDeleteValueW@8
	RegEnumKeyA@16
	RegEnumKeyExA@32
	RegEnumKeyExW@32
	RegEnumKeyW@16
	RegEnumValueA@32
	RegEnumValueW@32
	RegFlushKey@4
	RegGetKeySecurity@16
	RegLoadKeyA@12
	RegLoadKeyW@12
	RegNotifyChangeKeyValue@20
	RegOpenKeyA@12
DONE	RegOpenKeyExA@20
	RegOpenKeyExW@20
	RegOpenKeyW@12
	RegQueryInfoKeyA@48
	RegQueryInfoKeyW@48
DONE	RegQueryValueA@16
DONE	RegQueryValueExA@24
	RegQueryValueExW@24
	RegQueryValueW@16
	RegReplaceKeyA@16
	RegReplaceKeyW@16
	RegRestoreKeyA@12
	RegRestoreKeyW@12
	RegSaveKeyA@12
	RegSaveKeyW@12
	RegSetKeySecurity@12
	RegSetValueA@20
DONE	RegSetValueExA@24
	RegSetValueExW@24
	RegSetValueW@20
	RegUnLoadKeyA@8
	RegUnLoadKeyW@8
	RegisterEventSourceA@8
	RegisterEventSourceW@8
	RegisterServiceCtrlHandlerA@8
	RegisterServiceCtrlHandlerW@8
	ReportEventA@36
	ReportEventW@36
	RevertToSelf@0
	SetAclInformation@16
	SetFileSecurityA@12
	SetFileSecurityW@12
	SetKernelObjectSecurity@12
	SetPrivateObjectSecurity@20
	SetSecurityDescriptorDacl@16
	SetSecurityDescriptorGroup@12
	SetSecurityDescriptorOwner@12
	SetSecurityDescriptorSacl@16
	SetServiceBits@16
	SetServiceObjectSecurity@12
	SetServiceStatus@8
	SetThreadToken@8
	SetTokenInformation@16
	StartServiceA@12
	StartServiceCtrlDispatcherA@4
	StartServiceCtrlDispatcherW@4
	StartServiceW@12
	SynchronizeWindows31FilesAndWindowsNTRegistry@16
	SystemFunction001@12
	SystemFunction002@12
	SystemFunction003@8
	SystemFunction004@12
	SystemFunction005@12
	SystemFunction006@8
	SystemFunction007@8
	SystemFunction008@12
	SystemFunction009@12
	SystemFunction010@12
	SystemFunction011@12
	SystemFunction012@12
	SystemFunction013@12
	SystemFunction014@12
	SystemFunction015@12
	SystemFunction016@12
	SystemFunction017@12
	SystemFunction018@12
	SystemFunction019@12
	SystemFunction020@12
	SystemFunction021@12
	SystemFunction022@12
	SystemFunction023@12
	SystemFunction024@12
	SystemFunction025@12
	SystemFunction026@12
	SystemFunction027@12
	SystemFunction028@8
	SystemFunction029@8
	SystemFunction030@8
	SystemFunction031@8
	SystemFunction032@8
	SystemFunction033@8
	UnlockServiceDatabase@4

*/

#define GetUserName GetUserNameA
#define RegOpenKeyEx RegOpenKeyExA
#define RegQueryValueEx RegQueryValueExA
#define RegCreateKeyEx  RegCreateKeyExA



#define REGSAM long
BOOL WINAPI GetUserNameA (char * lpBuffer, DWORD  *nSize);

DECLARE_HANDLE(HKEY);

/* How does all this stuff work on 64 bit machines ? */

/* Builtin handles for keys */

#define HKEY_CLASSES_ROOT           ((HKEY) 0x80000000)
#define HKEY_CURRENT_USER           ((HKEY) 0x80000001)
#define HKEY_LOCAL_MACHINE          ((HKEY) 0x80000002)
#define HKEY_USERS                  ((HKEY) 0x80000003)

/* Options for creating keys */
#define REG_OPTION_NON_VOLATILE     0
#define REG_OPTION_VOLATILE         1

/* Disposition results */
#define REG_CREATED_NEW_KEY         1
#define REG_OPENED_EXISTING_KEY     2

/* Bits for sam, who ever he is. */

#define KEY_QUERY_VALUE         0x01
#define KEY_ENUMERATE_SUB_KEYS  0x08
#define KEY_NOTIFY              0x10
#define KEY_CREATE_SUB_KEY      0x04
#define KEY_CREATE_LINK         0x20
#define KEY_SET_VALUE           0x02


/* Types of keys */

#define REG_SZ			1 
#define REG_EXPAND_SZ		2
#define REG_BINARY		3
#define REG_DWORD		4
#define REG_DWORD_LITTLE_ENDIAN 4
#define REG_DWORD_BIG_ENDIAN	5
#define REG_LINK		6
#define REG_MULTI_SZ		7


#define KEY_ALL_ACCESS KEY_QUERY_VALUE|KEY_ENUMERATE_SUB_KEYS|KEY_NOTIFY|KEY_CREATE_SUB_KEY|\
                KEY_CREATE_LINK | KEY_SET_VALUE
#define KEY_READ KEY_QUERY_VALUE|KEY_ENUMERATE_SUB_KEYS|KEY_NOTIFY
#define KEY_WRITE KEY_SET_VALUE|KEY_CREATE_SUB_KEY

WINAPI LONG RegOpenKeyA(HKEY key, const char *subkey, HKEY *res);
WINAPI LONG RegOpenKeyExA(HKEY key, const char *subkey, DWORD options, long reg, HKEY *res);
WINAPI LONG RegQueryValueA(HKEY hKey, const char *subkey,char *res,LONG *res_len);

WINAPI LONG RegCreateKeyExA(HKEY key, 
			    const char *subkey,
			    long dontuse, 
			    const char *keyclass,
			    DWORD options,
			    REGSAM sam,
			    SECURITY_ATTRIBUTES *atts,
			    HKEY *res,
			    DWORD *disp);
WINAPI BOOL RegCloseKey(HKEY);

WINAPI
LONG
RegSetValueExA (HKEY key,
		const char *name,
		DWORD dontuse,
		DWORD type,
		const void* data,
		DWORD len
		);




WINAPI LONG RegQueryValueExA(HKEY key,
			     const char *subkey,
			     DWORD dontuse,
			     DWORD *type,
			     void *ptr,
			     DWORD *len);

WINAPI LONG RegDeleteKeyA(HKEY, const char *name);
