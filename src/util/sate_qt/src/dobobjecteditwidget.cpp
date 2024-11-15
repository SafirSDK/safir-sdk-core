/******************************************************************************
*
* Copyright Saab AB, 2024 (http://safirsdkcore.com)
*
* Created by: Joel Ottosson
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
#include "dobobjecteditwidget.h"
#include "ui_dobobjecteditwidget.h"
#include "dobobjectmodel.h"
#include "dobobjectdelegate.h"
#include "dobobjectbuilder.h"
#include <QTreeView>
#include <QKeyEvent>
#include <QTimer>
#include <Safir/Dob/Typesystem/Serialization.h>

DobObjectEditWidget::DobObjectEditWidget(DobInterface* dob, int64_t typeId, QWidget *parent)
    : QWidget(parent)
    , ui(new Ui::DobObjectEditWidget)
    , m_dob(dob)
    , m_typeId(typeId)
{
    Init(new DobObjectModel(m_typeId, this));
}

DobObjectEditWidget::DobObjectEditWidget(DobInterface* dob, int64_t typeId, QString channelHandler,
                             int64_t instance, const Safir::Dob::Typesystem::ObjectPtr& object,  QWidget *parent)
    : QWidget(parent)
    , ui(new Ui::DobObjectEditWidget)
    , m_dob(dob)
    , m_typeId(typeId)
{
    Init(new DobObjectModel(m_typeId, object, this));
    ui->operationsWidget->idEdit->setText(channelHandler);
    ui->operationsWidget->instanceEdit->setText(QString::number(instance));
}

DobObjectEditWidget::~DobObjectEditWidget()
{
    delete ui;
}

void DobObjectEditWidget::Init(DobObjectModel* model)
{
    ui->setupUi(this);

    ui->objectEditTreeView->setModel(model);
    ui->objectEditTreeView->setItemDelegateForColumn(1, new DobObjectDelegate());
    ui->objectEditTreeView->setItemDelegateForColumn(3, new CheckboxDelegate());

    ui->objectEditTreeView->setColumnWidth(0, 400);
    ui->objectEditTreeView->setColumnWidth(1, 400);
    ui->objectEditTreeView->header()->setSectionResizeMode(2, QHeaderView::ResizeToContents);
    ui->objectEditTreeView->header()->setSectionResizeMode(3, QHeaderView::ResizeToContents);
    ui->objectEditTreeView->header()->setStretchLastSection(true);

    // Disable built in edit triggers and handle it manually below.
    ui->objectEditTreeView->setEditTriggers(QAbstractItemView::NoEditTriggers);

    // Eventfilter that enters edit mode for selected row when pressing return key.
    ui->objectEditTreeView->installEventFilter(this);

    // Make single click enter edit mode.
    connect(ui->objectEditTreeView, &QTreeView::clicked, this, [this](const QModelIndex& index)
    {
        if (!index.isValid())
            return;

        auto p = static_cast<const MemberTreeItem*>(index.internalPointer());
        
        // Array root elements have no edit mode
        if (p->IsContainerRootItem() && p->GetMemberInfo()->collectionType == ArrayCollectionType)
        {
            return;
        }

        if (index.column() == 3) // Edit change flag when click column 3
        {
            ui->objectEditTreeView->edit(index);
        }
        else // Click elsewhere on a row will edit value
        {
            EditValue(index);
        }
    });

    connect(model, &DobObjectModel::OpenEditor, this, [this](const QModelIndex& index)
    {
        // For some reason, sometimes the node we want to edit will not expand unless we do a collpse-expand before enter edit mode.
        // We also queue up the calls to let any currently open editor close before we open a new one. If not Qt outputs error messages.
        QMetaObject::invokeMethod(qApp, [this, index]{ui->objectEditTreeView->collapse(index.parent());}, Qt::QueuedConnection);
        QMetaObject::invokeMethod(qApp, [this, index]{ui->objectEditTreeView->expand(index.parent());}, Qt::QueuedConnection);
        QMetaObject::invokeMethod(qApp, [this, index]{EditValue(index);}, Qt::QueuedConnection);
    });

    // Expand containers when a child item is added.
    connect(model, &QAbstractItemModel::rowsInserted, this, [this](const QModelIndex &parent, int /*first*/, int /*last*/){
        ui->objectEditTreeView->expand(parent);
    });

    // Setup operation buttons
    ui->operationsWidget->SetConfiguration(TypesystemRepository::Instance().GetClass(m_typeId)->dobBaseClass);
    connect(ui->operationsWidget->setChangesBtn, &QPushButton::clicked, this, [this]
    {
        auto obj = BuildObject();
        auto ent = std::dynamic_pointer_cast<Safir::Dob::Entity>(obj);
        m_dob->SetChanges(ent, ui->operationsWidget->Instance(), ui->operationsWidget->Handler());
    });
    connect(ui->operationsWidget->setAllBtn, &QPushButton::clicked, this,  [this]
    {
        auto obj = BuildObject();
        auto ent = std::dynamic_pointer_cast<Safir::Dob::Entity>(obj);
        m_dob->SetAll(ent, ui->operationsWidget->Instance(), ui->operationsWidget->Handler());
    });
    connect(ui->operationsWidget->deleteBtn, &QPushButton::clicked, this, [this]
    {
        m_dob->Delete(sdt::EntityId(m_typeId, ui->operationsWidget->Instance()), ui->operationsWidget->Handler());
    });
    connect(ui->operationsWidget->createReqBtn, &QPushButton::clicked, this, [this]
    {
        auto obj = BuildObject();
        auto ent = std::dynamic_pointer_cast<Safir::Dob::Entity>(obj);
        m_dob->CreateRequest(ent, ui->operationsWidget->Instance(), ui->operationsWidget->Handler());
    });
    connect(ui->operationsWidget->updateReqBtn, &QPushButton::clicked, this, [this]
    {
        auto obj = BuildObject();
        auto ent = std::dynamic_pointer_cast<Safir::Dob::Entity>(obj);
        m_dob->UpdateRequest(ent, ui->operationsWidget->Instance());
    });
    connect(ui->operationsWidget->deleteReqBtn, &QPushButton::clicked, this, [this]
    {
        m_dob->DeleteRequest(sdt::EntityId(m_typeId, ui->operationsWidget->Instance()));
    });
    connect(ui->operationsWidget->sendMsgBtn, &QPushButton::clicked, this, [this]
    {
        auto obj = BuildObject();
        auto msg = std::dynamic_pointer_cast<Safir::Dob::Message>(obj);
        m_dob->SendMessage(msg, ui->operationsWidget->Channel());
    });
    connect(ui->operationsWidget->sendReqBtn, &QPushButton::clicked, this, [this]
    {
        auto obj = BuildObject();
        auto req = std::dynamic_pointer_cast<Safir::Dob::Service>(obj);
        m_dob->SendServiceRequest(req, ui->operationsWidget->Handler());
    });
    connect(ui->operationsWidget->toXmlBtn, &QPushButton::clicked, this, [this]
    {
        auto obj = BuildObject();
        auto xml = QString::fromStdWString(Safir::Dob::Typesystem::Serialization::ToXml(obj));
        emit XmlSerializedObject(QString::fromStdWString(sdt::Operations::GetName(m_typeId)), xml);
    });
    connect(ui->operationsWidget->toJsonBtn, &QPushButton::clicked, this, [this]
    {
        auto obj = BuildObject();
        auto json = QString::fromStdWString(Safir::Dob::Typesystem::Serialization::ToJson(obj));
        emit JsonSerializedObject(QString::fromStdWString(sdt::Operations::GetName(m_typeId)), json);
    });
}

void DobObjectEditWidget::EditValue(const QModelIndex& index)
{
    if (index.isValid())
    {
        // Click elsewhere on a row will edit value
        auto valIndex = ui->objectEditTreeView->model()->index(index.row(), 1, index.parent());
        ui->objectEditTreeView->setCurrentIndex(valIndex); // This line is needed to make tab open next editor correctly.
        ui->objectEditTreeView->edit(valIndex);
    }
}

bool DobObjectEditWidget::eventFilter(QObject* /*object*/, QEvent *event)
{
    if (event->type() == QEvent::KeyPress)
    {
        QKeyEvent* key = static_cast<QKeyEvent*>(event);
        if (key->key()==Qt::Key_Enter || key->key()==Qt::Key_Return)
        {
            EditValue(ui->objectEditTreeView->currentIndex());
            return true;
        }
    }
    return false;
}

Safir::Dob::Typesystem::ObjectPtr DobObjectEditWidget::BuildObject() const
{
    auto root = static_cast<DobObjectModel*>(ui->objectEditTreeView->model())->InvisibleRoot();
    DobObjectBuilder builder;
    auto obj = builder.CreateObject(root);
    return obj;
}
