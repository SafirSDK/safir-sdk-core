/******************************************************************************
*
* Copyright Saab AB, 2005-2008 (http://www.safirsdk.com)
*
* Created by: Jörgen Johansson / stjrjo
*
*******************************************************************************
*
* This file is part of Safir SDK Core.
*
* Safir SDK Core is free software: you can redistribute it and/or modify
* it under the terms of version 3 of the GNU General Public License as
* published by the Free Software Foundation.
*
* Safir SDK Core is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with Safir SDK Core.  If not, see <http://www.gnu.org/licenses/>.
*
******************************************************************************/
#if !defined(OLIB_UNIT_ACCESS_H)
#define OLIB_UNIT_ACCESS_H

#include <Safir/Databases/Odbc/Defs.h>
#include <Safir/Databases/Odbc/Statement.h>
#include <Safir/Databases/Odbc/Connection.h>
#include <Safir/Databases/Odbc/Environment.h>
#include <Safir/Databases/Odbc/OutputParameter.h>
#include <Safir/Databases/Odbc/Columns.h>
#include <Safir/Databases/Odbc/InputParameter.h>
#include <Safir/Databases/Odbc/InputOutputParameter.h>
#include <Safir/Dob/Typesystem/Defs.h>
#include <Safir/Dob/Connection.h>
#include <Safir/Dob/Consumer.h>

class DbUnitAccess : public Safir::Dob::Dispatcher, public Safir::Dob::StopHandler
{
private:
    Safir::Databases::Odbc::Int32OutputParameter        m_outParamUnitId;
    Safir::Databases::Odbc::WideStringOutputParameter   m_outParamCallsign;
    Safir::Databases::Odbc::WideStringOutputParameter   m_outParamUnitSizeId;
    Safir::Databases::Odbc::WideStringOutputParameter   m_outParamUnitIdentityId;
    Safir::Databases::Odbc::Int32OutputParameter        m_outParamCombatReadiness;
    Safir::Databases::Odbc::WideStringOutputParameter   m_outParamCombatReadinessDescription;
    Safir::Databases::Odbc::Float64OutputParameter      m_outParamLatitude;
    Safir::Databases::Odbc::Float64OutputParameter      m_outParamLongitude;
    Safir::Databases::Odbc::Float32OutputParameter      m_outParamCourse;
    Safir::Databases::Odbc::Float32OutputParameter      m_outParamSpeed;
    Safir::Databases::Odbc::TimeOutputParameter         m_outParamMeasurementTime;
    Safir::Databases::Odbc::BooleanOutputParameter      m_outParamIsAlive;
    Safir::Databases::Odbc::Int64OutputParameter        m_outParamAlargeinteger;

    Safir::Databases::Odbc::Int32Parameter          m_paramUnitId;
    Safir::Databases::Odbc::WideStringParameter     m_paramCallsign;
    Safir::Databases::Odbc::WideStringParameter     m_paramUnitSizeId;
    Safir::Databases::Odbc::WideStringParameter     m_paramUnitIdentityId;
    Safir::Databases::Odbc::Int32Parameter          m_paramCombatReadiness;
    Safir::Databases::Odbc::WideStringParameter     m_paramCombatReadinessDescription;
    Safir::Databases::Odbc::Float64Parameter        m_paramLatitude;
    Safir::Databases::Odbc::Float64Parameter        m_paramLongitude;
    Safir::Databases::Odbc::Float32Parameter        m_paramCourse;
    Safir::Databases::Odbc::Float32Parameter        m_paramSpeed;
    Safir::Databases::Odbc::TimeParameter           m_paramMeasurementTime;
    Safir::Databases::Odbc::WideStringParameter     m_paramUnitTypeId;
    Safir::Databases::Odbc::BooleanParameter        m_paramIsAlive;
    Safir::Databases::Odbc::Int64Parameter          m_paramAlargeinteger;

    Safir::Databases::Odbc::Int64Parameter      m_paramTypeId;
    Safir::Databases::Odbc::Int32Parameter      m_paramInstanceNo;
    Safir::Databases::Odbc::WideStringParameter m_paramData;

    Safir::Databases::Odbc::Int32Column         m_columnUnitId;
    Safir::Databases::Odbc::WideStringColumn    m_columnCallsign;
    Safir::Databases::Odbc::WideStringColumn    m_columnUnitSizeId;
    Safir::Databases::Odbc::WideStringColumn    m_columnUnitIdentityId;
    Safir::Databases::Odbc::Int32Column         m_columnCombatReadiness;
    Safir::Databases::Odbc::WideStringColumn    m_columnCombatReadinessDescription;
    Safir::Databases::Odbc::Float64Column       m_columnLatitude;
    Safir::Databases::Odbc::Float64Column       m_columnLongitude;
    Safir::Databases::Odbc::Float32Column       m_columnCourse;
    Safir::Databases::Odbc::Float32Column       m_columnSpeed;
    Safir::Databases::Odbc::TimeColumn          m_columnMeasurementTime;
    Safir::Databases::Odbc::WideStringColumn    m_columnUnitTypeId;
    Safir::Databases::Odbc::BooleanColumn       m_columnIsAlive;
    Safir::Databases::Odbc::Int64Column         m_columnAlargeinteger;
    
    Safir::Databases::Odbc::LongWideStringParameter m_paramNClob;
    Safir::Databases::Odbc::WideStringColumn        m_columnNClob;

    Safir::Databases::Odbc::BinaryParameter         m_paramBinary;
    Safir::Databases::Odbc::BinaryColumn            m_columnBinary;

    Safir::Databases::Odbc::LongBinaryParameter m_paramBlob;
    Safir::Databases::Odbc::BinaryColumn        m_columnBlob;

    Safir::Databases::Odbc::Int32InputOutputParameter       m_inoutParamUnitId;
    Safir::Databases::Odbc::WideStringInputOutputParameter  m_inoutParamCallsign;
    Safir::Databases::Odbc::WideStringInputOutputParameter  m_inoutParamUnitSizeId;
    Safir::Databases::Odbc::WideStringInputOutputParameter  m_inoutParamUnitIdentityId;
    Safir::Databases::Odbc::Int32InputOutputParameter       m_inoutParamCombatReadiness;
    Safir::Databases::Odbc::WideStringInputOutputParameter  m_inoutParamCombatReadinessDescription;
    Safir::Databases::Odbc::Float64InputOutputParameter     m_inoutParamLatitude;
    Safir::Databases::Odbc::Float64InputOutputParameter     m_inoutParamLongitude;
    Safir::Databases::Odbc::Float32InputOutputParameter     m_inoutParamCourse;
    Safir::Databases::Odbc::Float32InputOutputParameter     m_inoutParamSpeed;
    Safir::Databases::Odbc::TimeInputOutputParameter        m_inoutParamMeasurementTime;
    Safir::Databases::Odbc::BooleanInputOutputParameter     m_inoutParamIsAlive;
    Safir::Databases::Odbc::Int64InputOutputParameter       m_inoutParamAlargeinteger;

    long m_nNoOfErrors;

    Safir::Databases::Odbc::Connection     m_connection;
    Safir::Databases::Odbc::Environment    m_environment;

    Safir::Databases::Odbc::Statement m_InputOutputStmt;
    bool m_bInputOutputIsPrepared;
    Safir::Databases::Odbc::Statement m_OutputStmt;
    bool m_bOutputIsPrepared;
    Safir::Databases::Odbc::Statement m_CreateStmt;
    bool m_bCreateIsPrepared;
    Safir::Databases::Odbc::Statement m_UpdateStmt;
    bool m_bUpdateIsPrepared;
    Safir::Databases::Odbc::Statement m_DeleteStmt;
    bool m_bDeleteIsPrepared;
    Safir::Databases::Odbc::Statement m_GetAllUnitsStmt;
    bool m_bGetAllUnitsIsPrepared;
    Safir::Databases::Odbc::Statement m_PerfTestStmt;
    bool m_bPerfTestIsPrepared;
    Safir::Databases::Odbc::Statement m_UseDbStmt;
    bool m_bUseDbIsPrepared;
    Safir::Databases::Odbc::Statement m_BinaryReadStmt;
    bool m_bBinaryReadIsPrepared;
    Safir::Databases::Odbc::Statement m_BinaryWriteStmt;
    bool m_bBinaryWriteIsPrepared;
    Safir::Databases::Odbc::Statement m_WriteNClobStmt;
    bool m_bWriteNClobIsPrepared;
    Safir::Databases::Odbc::Statement m_ReadNClobStmt;
    bool m_bReadNClobIsPrepared;
    Safir::Databases::Odbc::Statement m_WriteBlobStmt;
    bool m_bWriteBlobIsPrepared;
    Safir::Databases::Odbc::Statement m_ReadBlobStmt;
    bool m_bReadBlobIsPrepared;

    DbUnitAccess(const DbUnitAccess &); // Disable copy constructor
    const DbUnitAccess & operator= (const DbUnitAccess & d);    // Disable assignment operator 

    Safir::Dob::Connection m_dob;
    bool bDobIsConnected;
    static char * BinaryData;
public:
    DbUnitAccess();
    virtual ~DbUnitAccess();

    void UseOlibTest();
    void PerfTest();

    void SetReadAllTimeout();
    long GetReadAllTimeout();

    void SetConnectionTimeout();
    long GetConnectionTimeout();

    void SetConnectionPooling();
    void GetConnectionPooling();

    void TestInputOutputParameters();
    void TestOutputParameters();
    void CreateUnit();
    void UpdateUnit();
    void DeleteUnit();
    void ReadAllUnits();
    void BinaryTestRead();
    void BinaryTestWrite();
    void WriteNClobs();
    void ReadNClobs();
    void WriteBlob();
    void ReadBlob();

    long GetNoOfErrors() { return m_nNoOfErrors; }
    void ClearNoOfErrors() { m_nNoOfErrors = 0; }

    void AllocStmt();
    void CloseStmt();

    void Connect();
    void Disconnect();

protected:
    void OnDoDispatch();
    void OnStopOrder();
};

#endif // OLIB_UNIT_ACCESS_H
