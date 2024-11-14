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
#include "dobobjectdelegate.h"
#include "valueinput.h"
#include <QCheckBox>
#include <QComboBox>
#include <QEvent>
#include <QHBoxLayout>
#include <QKeyEvent>
#include <QLineEdit>
#include <QValidator>

namespace
{
const QValidator* CreateValidator(Safir::Dob::Typesystem::MemberType memberType, QWidget *parent)
{
    switch (memberType)
    {
    case EntityIdMemberType:
    {
        // EntityId are written as "TypeId : Instance" or just "TypeId Instance". No check that typeId exists.
        QRegularExpression rx("[0-9A-Za-z\\s]+[:\\s][0-9A-Za-z\\s]+");
        return new QRegularExpressionValidator(rx, parent);
    }
    break;

    case Int32MemberType:
    {
        return new QIntValidator(parent);
    }
    break;

    case Int64MemberType:
    {
        QRegularExpression rx("[-+]?\\d{1,}");
        return new QRegularExpressionValidator(rx, parent);
    }
    break;

    case Float32MemberType:
    case Ampere32MemberType:
    case CubicMeter32MemberType:
    case Hertz32MemberType:
    case Joule32MemberType:
    case Kelvin32MemberType:
    case Kilogram32MemberType:
    case Meter32MemberType:
    case MeterPerSecond32MemberType:
    case MeterPerSecondSquared32MemberType:
    case Newton32MemberType:
    case Pascal32MemberType:
    case Radian32MemberType:
    case RadianPerSecond32MemberType:
    case RadianPerSecondSquared32MemberType:
    case Second32MemberType:
    case SquareMeter32MemberType:
    case Steradian32MemberType:
    case Volt32MemberType:
    case Watt32MemberType:
    {
        return new QDoubleValidator(parent);
    }
    break;

    case Float64MemberType:
    case Ampere64MemberType:
    case CubicMeter64MemberType:
    case Hertz64MemberType:
    case Joule64MemberType:
    case Kelvin64MemberType:
    case Kilogram64MemberType:
    case Meter64MemberType:
    case MeterPerSecond64MemberType:
    case MeterPerSecondSquared64MemberType:
    case Newton64MemberType:
    case Pascal64MemberType:
    case Radian64MemberType:
    case RadianPerSecond64MemberType:
    case RadianPerSecondSquared64MemberType:
    case Second64MemberType:
    case SquareMeter64MemberType:
    case Steradian64MemberType:
    case Volt64MemberType:
    case Watt64MemberType:
    {
        return new QDoubleValidator(parent);
    }
    break;

    default:
    {
        // No validator
        return nullptr;
    }
    }
}

ValueInput* CreateSingleEditor(Safir::Dob::Typesystem::MemberType memberType, int64_t memberTypeId, bool nullValue, const QString& deleteBtnText, QWidget *parent)
{
    // ComboBox value input
    if (memberType == BooleanMemberType)
    {
        return new ComboBoxValueInput({"true", "false"}, nullValue, deleteBtnText, parent);
    }

    if (memberType == EnumerationMemberType)
    {
        auto en = TypesystemRepository::Instance().GetEnum(memberTypeId);
        return new ComboBoxValueInput(en->values, nullValue, deleteBtnText, parent);
    }

    if (memberType == ObjectMemberType)
    {
        auto typeIds = Safir::Dob::Typesystem::Operations::GetClassTree(memberTypeId);
        std::vector<QString> typeNames;
        for (auto tid : typeIds)
        {
            typeNames.push_back(QString::fromStdWString(Safir::Dob::Typesystem::Operations::GetName(tid)));
        }
        return new ComboBoxValueInput(typeNames, nullValue, deleteBtnText, parent);
    }

    // Text value input
    auto editor = new TextValueInput(nullValue, deleteBtnText, parent);
    editor->SetValidator(CreateValidator(memberType, parent));
    return editor;
}

}


DobObjectDelegate::DobObjectDelegate() {}

QWidget* DobObjectDelegate::createEditor(QWidget *parent,
                                    const QStyleOptionViewItem &option,
                                    const QModelIndex &index) const

{
    if (index.isValid())
    {
        auto item = static_cast<MemberTreeItem*>(index.internalPointer());
        ValueInput* editor = nullptr;
        if (item->GetMemberInfo()->collectionType == SequenceCollectionType)
        {
            if (item->IsContainerRootItem())
            {
                editor = CreateSingleEditor(item->GetMemberInfo()->memberType, item->GetMemberInfo()->memberTypeId, false, "Clear", parent);
            }
            else
            {
                editor = CreateSingleEditor(item->GetMemberInfo()->memberType, item->GetMemberInfo()->memberTypeId, false, "Delete", parent);
            }
        }
        else if (item->GetMemberInfo()->collectionType == DictionaryCollectionType)
        {
            if (item->IsContainerRootItem())
            {
                // Create editor for dictionary key
                editor = CreateSingleEditor(item->GetMemberInfo()->keyType, item->GetMemberInfo()->keyTypeId, false, "Clear", parent);
            }
            else
            {
                // Create editor for dictionary value
                editor = CreateSingleEditor(item->GetMemberInfo()->memberType, item->GetMemberInfo()->memberTypeId, true, "Delete", parent);
            }
        }
        else
        {
            editor = CreateSingleEditor(item->GetMemberInfo()->memberType, item->GetMemberInfo()->memberTypeId, true, QString(), parent);
        }

        if (editor != nullptr)
        {
            editor->setMinimumHeight(24);
            editor->setMaximumHeight(24);
            connect(editor, &ValueInput::Commit, this, &DobObjectDelegate::CommitAndCloseEditor, Qt::QueuedConnection);
            return editor;
        }
    }

    return QStyledItemDelegate::createEditor(parent, option, index);
}

void DobObjectDelegate::setEditorData(QWidget* editor, const QModelIndex& index) const
{
    if (index.isValid() && editor != nullptr)
    {
        auto item = static_cast<MemberTreeItem*>(index.internalPointer());

        if (item->IsContainerRootItem())
        {
            // Adding new values to a sequence or dictionary, always start with empty input.
            return;
        }

        auto val = static_cast<ValueInput*>(editor);
        if (item->IsNull())
        {
            val->SetNull();
        }
        else
        {
            val->SetValue(item->GetValue());
        }
    }
}

void DobObjectDelegate::setModelData(QWidget* editor, QAbstractItemModel* model, const QModelIndex& index) const
{
    if (index.isValid() && editor != nullptr)
    {
        auto val = static_cast<ValueInput*>(editor);
        if (val->IsDelete())
        {
            model->setData(index, {}, MemberTreeItem::DeleteItemRole);
        }
        else if (val->IsNull())
        {
            model->setData(index, {}); // EditRole - setNull
        }
        else
        {
            model->setData(index, val->GetValue()); // EditRole - setValue
        }
    }
}

void DobObjectDelegate::CommitAndCloseEditor(int nextRow)
{
    auto editor = static_cast<QWidget*>(sender());
    emit commitData(editor);

    if (nextRow == 1)
    {
        emit closeEditor(editor, QStyledItemDelegate::EditNextItem);
    }
    else if (nextRow == -1)
    {
        emit closeEditor(editor, QStyledItemDelegate::EditPreviousItem);
    }
    else
    {
        emit closeEditor(editor, QStyledItemDelegate::NoHint);
    }
}

//---------------------------------------
// CheckboxDelegate
//---------------------------------------
CheckboxDelegate::CheckboxDelegate() {}

QWidget* CheckboxDelegate::createEditor(QWidget *parent,
                                         const QStyleOptionViewItem &option,
                                         const QModelIndex &index) const

{
    if (index.isValid())
    {
        return new QCheckBox(parent);
    }

    return CheckboxDelegate::createEditor(parent, option, index);
}

void CheckboxDelegate::setEditorData(QWidget* editor, const QModelIndex& index) const
{
    if (index.isValid() && editor != nullptr)
    {
        auto checked = !index.data().toString().isEmpty();
        static_cast<QCheckBox*>(editor)->setChecked(checked);
    }
}

void CheckboxDelegate::setModelData(QWidget* editor, QAbstractItemModel* model, const QModelIndex& index) const
{
    if (index.isValid() && editor != nullptr)
    {
        auto val = static_cast<QCheckBox*>(editor)->isChecked();
        model->setData(index, val);
    }
}

void CheckboxDelegate::initStyleOption(QStyleOptionViewItem *option, const QModelIndex &index) const
{
    QStyledItemDelegate::initStyleOption(option, index);
    option->decorationAlignment = Qt::AlignHCenter | Qt::AlignCenter;
}


