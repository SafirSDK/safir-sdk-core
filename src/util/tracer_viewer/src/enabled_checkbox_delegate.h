/******************************************************************************
*
* Copyright Saab AB, 2025 (http://safirsdkcore.com)
*
* Created by: Lars Hagström / lars@foldspace.nu
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
#pragma once

#include <QStyledItemDelegate>
#include <QApplication>
#include <QStyleOptionButton>
#include <QMouseEvent>
#include <QKeyEvent>

class EnabledCheckBoxDelegate : public QStyledItemDelegate
{
    Q_OBJECT
public:
    using QStyledItemDelegate::QStyledItemDelegate;

    /* -------------------------------------------------------------------- */
    /* Painting                                                             */
    /* -------------------------------------------------------------------- */
    void initStyleOption(QStyleOptionViewItem* option,
                         const QModelIndex& index) const override
    {
        QStyledItemDelegate::initStyleOption(option, index);
        option->displayAlignment = Qt::AlignLeft | Qt::AlignVCenter;
    }

    /* -------------------------------------------------------------------- */
    /* Toggle on mouse/keyboard                                             */
    /* -------------------------------------------------------------------- */
    bool editorEvent(QEvent* event,
                     QAbstractItemModel* model,
                     const QStyleOptionViewItem& option,
                     const QModelIndex& index) override
    {
        if (!(index.flags() & Qt::ItemIsUserCheckable) ||
            !(index.flags() & Qt::ItemIsEnabled))
            return false;

        auto toggle = [&]()
        {
            const Qt::CheckState cur =
                static_cast<Qt::CheckState>(index.data(Qt::CheckStateRole).toInt());
            const Qt::CheckState nxt =
                (cur == Qt::Checked ? Qt::Unchecked : Qt::Checked);
            model->setData(index, nxt, Qt::CheckStateRole);
        };

        switch (event->type())
        {
        case QEvent::MouseButtonRelease:
        {
            auto* me = static_cast<QMouseEvent*>(event);
            if (option.rect.contains(me->pos()) && me->button() == Qt::LeftButton)
            {
                toggle();
                return true;
            }
            break;
        }
        case QEvent::KeyPress:
        {
            auto* ke = static_cast<QKeyEvent*>(event);
            if (ke->key() == Qt::Key_Space || ke->key() == Qt::Key_Select)
            {
                toggle();
                return true;
            }
            break;
        }
        default:
            break;
        }
        return false;
    }
};
