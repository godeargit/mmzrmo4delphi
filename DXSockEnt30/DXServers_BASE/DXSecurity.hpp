// Borland C++ Builder
// Copyright (c) 1995, 1999 by Borland International
// All rights reserved

// (DO NOT EDIT: machine generated header) 'DXSecurity.pas' rev: 5.00

#ifndef DXSecurityHPP
#define DXSecurityHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Dxsecurity
{
//-- type declarations -------------------------------------------------------
typedef unsigned HCERTSTORE;

struct TBLOB
{
	unsigned cbData;
	Byte *pbData;
} ;

typedef TBLOB  TCRYPT_INTEGER_BLOB;

typedef TBLOB *PCRYPT_INTEGER_BLOB;

typedef TBLOB  TCRYPT_UINT_BLOB;

typedef TBLOB *PCRYPT_UINT_BLOB;

typedef TBLOB  TCRYPT_OBJID_BLOB;

typedef TBLOB *PCRYPT_OBJID_BLOB;

typedef TBLOB  TCERT_NAME_BLOB;

typedef TBLOB *PCERT_NAME_BLOB;

typedef TBLOB  TCERT_RDN_VALUE_BLOB;

typedef TBLOB *PCERT_RDN_VALUE_BLOB;

typedef TBLOB  TCERT_BLOB;

typedef TBLOB *PCERT_BLOB;

typedef TBLOB  TCRL_BLOB;

typedef TBLOB *PCRL_BLOB;

typedef TBLOB  TDATA_BLOB;

typedef TBLOB *PDATA_BLOB;

typedef TBLOB  TCRYPT_DATA_BLOB;

typedef TBLOB *PCRYPT_DATA_BLOB;

typedef TBLOB  TCRYPT_HASH_BLOB;

typedef TBLOB *PCRYPT_HASH_BLOB;

typedef TBLOB  TCRYPT_DIGEST_BLOB;

typedef TBLOB *PCRYPT_DIGEST_BLOB;

typedef TBLOB  TCRYPT_DER_BLOB;

typedef TBLOB *PCRYPT_DER_BLOB;

typedef TBLOB  TCRYPT_ATTR_BLOB;

typedef TBLOB *PCRYPT_ATTR_BLOB;

struct TCRYPT_BIT_BLOB;
typedef TCRYPT_BIT_BLOB *PCRYPT_BIT_BLOB;

#pragma pack(push, 1)
struct TCRYPT_BIT_BLOB
{
	unsigned cbData;
	Byte *pbData;
	unsigned cUnusedBits;
} ;
#pragma pack(pop)

struct TCRYPT_ALGORITHM_IDENTIFIER;
typedef TCRYPT_ALGORITHM_IDENTIFIER *PCRYPT_ALGORITHM_IDENTIFIER;

#pragma pack(push, 1)
struct TCRYPT_ALGORITHM_IDENTIFIER
{
	AnsiString pszObjId;
	TBLOB Parameters;
} ;
#pragma pack(pop)

#pragma pack(push, 1)
struct TCERT_PUBLIC_KEY_INFO
{
	TCRYPT_ALGORITHM_IDENTIFIER Algorithm;
	TCRYPT_BIT_BLOB PublicKey;
} ;
#pragma pack(pop)

typedef TCERT_PUBLIC_KEY_INFO *PCERT_PUBLIC_KEY_INFO;

struct TCERT_EXTENSION;
typedef TCERT_EXTENSION *PCERT_EXTENSION;

#pragma pack(push, 1)
struct TCERT_EXTENSION
{
	AnsiString pszObjId;
	bool fCritical;
	TBLOB Value;
} ;
#pragma pack(pop)

struct TCERT_INFO;
typedef TCERT_INFO *PCERT_INFO;

#pragma pack(push, 1)
struct TCERT_INFO
{
	unsigned dwVersion;
	TBLOB SerialNumber;
	TCRYPT_ALGORITHM_IDENTIFIER SignatureAlgorithm;
	TBLOB Issuer;
	System::TDateTime NotBefore;
	System::TDateTime NotAfter;
	TBLOB Subject;
	TCERT_PUBLIC_KEY_INFO SubjectPublicKeyInfo;
	TCRYPT_BIT_BLOB IssuerUniqueId;
	TCRYPT_BIT_BLOB SubjectUniqueId;
	unsigned cExtension;
	TCERT_EXTENSION *rgExtension;
} ;
#pragma pack(pop)

struct TCERT_CONTEXT;
typedef TCERT_CONTEXT *PCERT_CONTEXT;

#pragma pack(push, 1)
struct TCERT_CONTEXT
{
	unsigned dwCertEncodingType;
	Byte *pbCertEncoded;
	unsigned cbCertEncoded;
	PCERT_INFO *pCertInfo;
	unsigned hCertStore;
} ;
#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
static const Shortint X509_ASN_ENCODING = 0x1;
static const Shortint PKCS_7_ASN_ENCODING = 0x2;
static const Shortint CERT_V1 = 0x1;
static const Shortint CERT_V2 = 0x2;
static const Shortint CERT_V3 = 0x3;
#define szOID_RSA "1.2.840.113549"
#define szOID_PKCS "1.2.840.113549.1"
#define szOID_RSA_HASH "1.2.840.113549.2"
#define szOID_RSA_ENCRYPT "1.2.840.113549.3"
#define szOID_PKCS_1 "1.2.840.113549.1.1"
#define szOID_PKCS_2 "1.2.840.113549.1.2"
#define szOID_PKCS_3 "1.2.840.113549.1.3"
#define szOID_PKCS_4 "1.2.840.113549.1.4"
#define szOID_PKCS_5 "1.2.840.113549.1.5"
#define szOID_PKCS_6 "1.2.840.113549.1.6"
#define szOID_PKCS_7 "1.2.840.113549.1.7"
#define szOID_PKCS_8 "1.2.840.113549.1.8"
#define szOID_PKCS_9 "1.2.840.113549.1.9"
#define szOID_PKCS_10 "1.2.840.113549.1.10"
#define szOID_PKCS_12 "1.2.840.113549.1.12"
#define szOID_RSA_MD2 "1.2.840.113549.1.1.2"
#define szOID_RSA_MD4 "1.2.840.113549.1.1.3"
#define szOID_RSA_MD5 "1.2.840.113549.1.1.4"
#define szOID_RSA_RSA "1.2.840.113549.1.1.1"
#define szOID_RSA_MD2RSA "1.2.840.113549.1.1.2"
#define szOID_RSA_MD4RSA "1.2.840.113549.1.1.3"
#define szOID_RSA_MD5RSA "1.2.840.113549.1.1.4"
#define szOID_RSA_SHA1RSA "1.2.840.113549.1.1.5"
#define szOID_RSA_SETOAEP_RSA "1.2.840.113549.1.1.5"
#define szOID_RSA_DH "1.2.840.113549.1.3.1"
#define szOID_RSA_data "1.2.840.113549.1.7.1"
#define szOID_RSA_signedData "1.2.840.113549.1.7.2"
#define szOID_RSA_envelopedData "1.2.840.113549.1.7.3"
#define szOID_RSA_signEnvData "1.2.840.113549.1.7.4"
#define szOID_RSA_digestedData "1.2.840.113549.1.7.5"
#define szOID_RSA_hashedData "1.2.840.113549.1.7.5"
#define szOID_RSA_encryptedData "1.2.840.113549.1.7.6"
#define szOID_RSA_emailAddr "1.2.840.113549.1.9.1"
#define szOID_RSA_unstructName "1.2.840.113549.1.9.2"
#define szOID_RSA_contentType "1.2.840.113549.1.9.3"
#define szOID_RSA_messageDigest "1.2.840.113549.1.9.4"
#define szOID_RSA_signingTime "1.2.840.113549.1.9.5"
#define szOID_RSA_counterSign "1.2.840.113549.1.9.6"
#define szOID_RSA_challengePwd "1.2.840.113549.1.9.7"
#define szOID_RSA_unstructAddr "1.2.840.113549.1.9.8"
#define szOID_RSA_extCertAttrs "1.2.840.113549.1.9.9"
#define szOID_RSA_SMIMECapabilities "1.2.840.113549.1.9.15"
#define szOID_RSA_preferSignedData "1.2.840.113549.1.9.15.1"
#define szOID_RSA_RC2CBC "1.2.840.113549.3.2"
#define szOID_RSA_RC4 "1.2.840.113549.3.4"
#define szOID_RSA_DES_EDE3_CBC "1.2.840.113549.3.7"
#define szOID_RSA_RC5_CBCPad "1.2.840.113549.3.9"
#define szOID_ANSI_x942 "1.2.840.10046"
#define szOID_ANSI_x942_DH "1.2.840.10046.2.1"
#define szOID_X957 "1.2.840.10040"
#define szOID_X957_DSA "1.2.840.10040.4.1"
#define szOID_DATA_STRUCTURE "1.2.840.10040.4.3"
#define szOId_DS "2.5"
#define szOID_DSALG "2.5.8"
#define szOID_DSALG_CRPT "2.5.8.1"
#define szOID_DSALG_HASH "2.5.8.2"
#define szOID_DSALG_SIGN "2.5.8.3"
#define szOID_DSALG_RSA "2.5.8.1.1"
#define szOID_OIW "1.3.14"
#define szOID_OIWSEC "1.3.14.3.2"
#define szOID_OIWSEC_md4RSA "1.3.14.3.2.2"
#define szOID_OIWSEC_md5RSA "1.3.14.3.2.3"
#define szOID_OIWSEC_md4RSA2 "1.3.14.3.2.4"
#define szOID_OIWSEC_desECB "1.3.14.3.2.6"
#define szOID_OIWSEC_desCBC "1.3.14.3.2.7"
#define szOID_OIWSEC_desOFB "1.3.14.3.2.8"
#define szOID_OIWSEC_desCFB "1.3.14.3.2.9"
#define szOID_OIWSEC_desMAC "1.3.14.3.2.10"
#define szOID_OIWSEC_rsaSign "1.3.14.3.2.11"
#define szOID_OIWSEC_dsa "1.3.14.3.2.12"
#define szOID_OIWSEC_shaDSA "1.3.14.3.2.13"
#define szOID_OIWSEC_mdc2RSA "1.3.14.3.2.14"
#define szOID_OIWSEC_shaRSA "1.3.14.3.2.15"
#define szOID_OIWSEC_dhCommMod "1.3.14.3.2.16"
#define szOID_OIWSEC_desEDE "1.3.14.3.2.17"
#define szOID_OIWSEC_sha "1.3.14.3.2.18"
#define szOID_OIWSEC_mdc2 "1.3.14.3.2.19"
#define szOID_OIWSEC_dsaComm "1.3.14.3.2.20"
#define szOID_OIWSEC_dsaCommSHA "1.3.14.3.2.21"
#define szOID_OIWSEC_rsaXchg "1.3.14.3.2.22"
#define szOID_OIWSEC_keyHashSeal "1.3.14.3.2.23"
#define szOID_OIWSEC_md2RSASign "1.3.14.3.2.24"
#define szOID_OIWSEC_md5RSASign "1.3.14.3.2.25"
#define szOID_OIWSEC_sha1 "1.3.14.3.2.26"
#define szOID_OIWSEC_dsaSHA1 "1.3.14.3.2.27"
#define szOID_OIWSEC_dsaCommSHA1 "1.3.14.3.2.28"
#define szOID_OIWSEC_sha1RSASign "1.3.14.3.2.29"
#define szOID_OIWDIR "1.3.14.7.2"
#define szOID_OIWDIR_CRPT "1.3.14.7.2.1"
#define szOID_OIWDIR_HASH "1.3.14.7.2.2"
#define szOID_OIWDIR_SIGN "1.3.14.7.2.3"
#define szOID_OIWDIR_md2 "1.3.14.7.2.2.1"
#define szOID_OIWDIR_md2RSA "1.3.14.7.2.3.1"
#define szOID_INFOSEC "2.16.840.1.101.2.1"
#define szOID_INFOSEC_sdnsSignature "2.16.840.1.101.2.1.1.1"
#define szOID_INFOSEC_mosaicSignature "2.16.840.1.101.2.1.1.2"
#define szOID_INFOSEC_sdnsConfidentiality "2.16.840.1.101.2.1.1.3"
#define szOID_INFOSEC_mosaicConfidentiality "2.16.840.1.101.2.1.1.4"
#define szOID_INFOSEC_sdnsIntegrity "2.16.840.1.101.2.1.1.5"
#define szOID_INFOSEC_mosaicIntegrity "2.16.840.1.101.2.1.1.6"
#define szOID_INFOSEC_sdnsTokenProtection "2.16.840.1.101.2.1.1.7"
#define szOID_INFOSEC_mosaicTokenProtection "2.16.840.1.101.2.1.1.8"
#define szOID_INFOSEC_sdnsKeyManagement "2.16.840.1.101.2.1.1.9"
#define szOID_INFOSEC_mosaicKeyManagement "2.16.840.1.101.2.1.1.10"
#define szOID_INFOSEC_sdnsKMandSig "2.16.840.1.101.2.1.1.11"
#define szOID_INFOSEC_mosaicKMandSig "2.16.840.1.101.2.1.1.12"
#define szOID_INFOSEC_SuiteASignature "2.16.840.1.101.2.1.1.13"
#define szOID_INFOSEC_SuiteAConfidentiality "2.16.840.1.101.2.1.1.14"
#define szOID_INFOSEC_SuiteAIntegrity "2.16.840.1.101.2.1.1.15"
#define szOID_INFOSEC_SuiteATokenProtection "2.16.840.1.101.2.1.1.16"
#define szOID_INFOSEC_SuiteAKeyManagement "2.16.840.1.101.2.1.1.17"
#define szOID_INFOSEC_SuiteAKMandSig "2.16.840.1.101.2.1.1.18"
#define szOID_INFOSEC_mosaicUpdatedSig "2.16.840.1.101.2.1.1.19"
#define szOID_INFOSEC_mosaicKMandUpdSig "2.16.840.1.101.2.1.1.20"
#define szOID_INFOSEC_mosaicUpdatedInteg "2.16.840.1.101.2.1.1.21"
#define X509_DSS_PARAMETERS "1.3.14.3.2.12"
#define PKCS_RC2_CBC_PARAMETERS "1.2.840.113549.3.2"
#define X509_OCTET_STRING "1.3.14.3.2.7"

}	/* namespace Dxsecurity */
#if !defined(NO_IMPLICIT_NAMESPACE_USE)
using namespace Dxsecurity;
#endif
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// DXSecurity
